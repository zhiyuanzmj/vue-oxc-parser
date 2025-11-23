pub fn is_simple_identifier(s: &str) -> bool {
  if s.is_empty() {
    return false;
  }
  let first = s.chars().next().unwrap();
  if !(first.is_ascii_alphabetic() || first == '_' || first == '$') {
    return false;
  }
  for c in s.chars().skip(1) {
    if !(c.is_ascii_alphanumeric()
      || c == '_'
      || c == '$'
      || (c as u32 >= 0x00A0 && c as u32 <= 0xFFFF))
    {
      return false;
    }
  }
  true
}
