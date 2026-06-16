package com.nickrobison.inland.executor.simd

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import java.lang.classfile.ClassFile
import java.lang.classfile.instruction.*
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.given
import scala.jdk.OptionConverters.RichOptional

class NoAllocationBytecodeTests extends AnyWordSpec with Matchers {

  private val InstancesPrefix = "com/nickrobison/inland/executor/instances/array"
  private val KernelsPrefix = "com/nickrobison/inland/executor/simd/kernels"

  private val analysedClasses: Seq[ClassAnalysis] = analysedClassFiles().map(analyseOnce)

  "simd classes" should {
    analysedClasses.foreach { ca =>
      val prefix = ca.className

      val newViolations = ca.objectAllocations.filterNot(isNewAllowed)
      if newViolations.isEmpty then
        s"$prefix: contain no NEW object allocations" in { succeed }
      else
        s"$prefix: contain no NEW object allocations" in {
          val report = newViolations.map((m, t) => s"  new $t in $m").mkString("\n")
          fail(s"Found ${newViolations.size} NEW allocation(s) in $prefix:\n$report")
        }

      val newArrViolations = ca.newArrayMethods.filterNot(isNewArrayAllowed(ca.className))
      if newArrViolations.isEmpty then
        s"$prefix: contain no newarray instructions" in { succeed }
      else
        s"$prefix: contain no newarray instructions" in {
          val report = newArrViolations.map(m => s"  newarray in $m").mkString("\n")
          fail(s"Found ${newArrViolations.size} newarray(s) in $prefix:\n$report")
        }

      if ca.newRefArrayMethods.isEmpty then
        s"$prefix: contain no anewarray instructions" in { succeed }
      else
        s"$prefix: contain no anewarray instructions" in {
          val report = ca.newRefArrayMethods.map(m => s"  anewarray in $m").mkString("\n")
          fail(s"Found ${ca.newRefArrayMethods.size} anewarray(s) in $prefix:\n$report")
        }

      val boxViolations = ca.boxingCalls.filterNot(_ => isBoxAllowed(ca.className))
      if boxViolations.isEmpty then
        s"$prefix: contain no boxing calls" in { succeed }
      else
        s"$prefix: contain no boxing calls" in {
          val report = boxViolations.map((m, t) => s"  box $t in $m").mkString("\n")
          fail(s"Found ${boxViolations.size} boxing call(s) in $prefix:\n$report")
        }
    }
  }

  private def isNewAllowed(methodDesc: String, target: String): Boolean = {
    // Module static initializer — runs once per JVM
    if methodDesc.startsWith("<clinit>") then return true
    // Serialization proxy — never in hot path
    if methodDesc.startsWith("writeReplace") then return true
    // Lazy val guard — one-time synchronization
    if target.startsWith("scala/runtime/LazyVals$") then return true
    if target == "scala/runtime/ModuleSerializationProxy" then return true
    // UnsupportedOperationException is thrown, not returned
    if target == "java/lang/UnsupportedOperationException" then return true
    // Algebra instances created in forSpecies — one-time setup, cached in lazy vals
    if methodDesc.contains("forSpecies") && target.endsWith("Algebra") then return true
    if target.endsWith("scalaVectorInstance") then return true
    if target.startsWith("com/nickrobison/inland") && target.endsWith("arrayVector") then return true
    false
  }

  private def isNewArrayAllowed(className: String)(methodDesc: String): Boolean =
    // fromVectorBatch/toVectorBatch have a scratch-array allocation in the
    // non-Array container fallback branch (case _).  Currently only Array[E]
    // containers exist, so this is dead code — but the bytecode is present.
    className.endsWith("Algebra") &&
    (methodDesc.startsWith("fromVectorBatch") || methodDesc.startsWith("toVectorBatch"))

  private def isBoxAllowed(className: String): Boolean =
    // Boxing inside AbstractVector bridge methods (generic→specialized boundary)
    // Unavoidable without specializing the Algebra hierarchy
    className.endsWith("Algebra") || className.contains("scala$package$")

  private def analyseOnce(classFile: File): ClassAnalysis = {
    val cf = ClassFile.of()
    val model = cf.parse(classFile.toPath)
    val className = model.thisClass().name().stringValue()

    var objectAllocations = List.empty[(String, String)]
    var newArrayMethods = List.empty[String]
    var newRefArrayMethods = List.empty[(String, String)]
    var boxingCalls = List.empty[(String, String)]

    model.methods().forEach { method =>
      val methodDesc = s"${method.methodName().stringValue()}${method.methodTypeSymbol().descriptorString()}"

      method.code().toScala.foreach { code =>
        code.elementStream().forEach { elem =>
          elem match {
            case newIns: NewObjectInstruction =>
              val target = newIns.className().name().stringValue()
              objectAllocations = (methodDesc -> target) :: objectAllocations
            case _: NewPrimitiveArrayInstruction =>
              newArrayMethods = methodDesc :: newArrayMethods
            case refArr: NewReferenceArrayInstruction =>
              val target = refArr.componentType().name().stringValue()
              newRefArrayMethods = (methodDesc -> target) :: newRefArrayMethods
            case multiArr: NewMultiArrayInstruction =>
              val target = multiArr.arrayType().name().stringValue()
              newRefArrayMethods = (methodDesc -> s"multi:$target") :: newRefArrayMethods
            case inv: InvokeInstruction =>
              val owner = inv.owner().name().stringValue()
              val name = inv.name().stringValue()
              if isBoxingCall(owner, name) then
                boxingCalls = (methodDesc -> s"$owner.$name") :: boxingCalls
            case _ => ()
          }
        }
      }
    }

    ClassAnalysis(
      className,
      objectAllocations = objectAllocations.reverse,
      newArrayMethods = newArrayMethods.reverse,
      newRefArrayMethods = newRefArrayMethods.reverse,
      boxingCalls = boxingCalls.reverse,
    )
  }

  private def isBoxingCall(owner: String, name: String): Boolean =
    (owner == "scala/runtime/BoxesRunTime" && (name.startsWith("boxTo") || name.startsWith("unboxTo"))) ||
    (owner.startsWith("java/lang/") && name == "valueOf")

  private def analysedClassFiles(): Seq[File] = {
    val allFiles = Seq(InstancesPrefix, KernelsPrefix).flatMap(classFilesUnder)
    allFiles.filterNot { f =>
      val n = f.getName
      n.contains("$$Lambda") || n.contains("$anon$") ||
      n.endsWith("Suite.class") || n.endsWith("Spec.class") ||
      n.endsWith("Laws.class") || n.endsWith("Test.class")
    }
  }

  private def classFilesUnder(packagePath: String): Seq[File] = {
    val loader = getClass.getClassLoader
    val url = loader.getResource(packagePath)
    if (url == null) {
      Seq.empty
    } else {
      val root = Paths.get(url.toURI)
      Files.walk(root).iterator().asScala
        .filter(_.toString.endsWith(".class"))
        .map(_.toFile)
        .toSeq
    }
  }
}

private case class ClassAnalysis(
  className: String,
  objectAllocations: Seq[(String, String)],
  newArrayMethods: Seq[String],
  newRefArrayMethods: Seq[(String, String)],
  boxingCalls: Seq[(String, String)],
)
