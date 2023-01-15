pub fn read_from_offset<T: Copy>(memory: &[u8], offset: usize) -> T {
  assert!(memory.len() >= std::mem::size_of::<T>() + offset);
  unsafe {*(memory[offset..].as_ptr() as *const T)}
}

pub fn mut_at_offset<T: Sized>(memory: &mut [u8], offset: usize) -> &mut T {
  assert!(memory.len() >= std::mem::size_of::<T>() + offset);
  unsafe {(memory[offset..].as_mut_ptr() as *mut T).as_mut().unwrap()}
}

pub fn read_next<T: Copy>(memory: &[u8]) -> T {
  read_from_offset(memory, 0)
}

pub fn read_after_opcode<T: Copy>(memory: &[u8]) -> T {
  read_from_offset(memory, 1)
}


pub fn pop_stack<T: Copy>(register: &mut super::Register, memory: &super::Heap) -> T {
  let res = read_from_offset(memory.as_slice(), register.SP as _);
  register.SP += std::mem::size_of::<T>() as u16;
  res
}

pub fn push_stack<T: Copy>(register: &mut super::Register, memory: &mut super::Heap, value: T) {
  register.SP -= std::mem::size_of::<T>() as u16;
  *mut_at_offset(memory.as_mut(), register.SP as _) = value; 
}


