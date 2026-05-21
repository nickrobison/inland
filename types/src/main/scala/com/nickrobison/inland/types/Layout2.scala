package com.nickrobison.inland.types

import java.lang.foreign.MemoryLayout

trait Layout2[T] {
  
  def layout: MemoryLayout

}
