awk -F'\t' 'NF>=2 {
  n = split($1, a, /[ \t]+/)
  for (i=1; i<=n; ++i) if (a[i] != "X") { printf "%s\t%s\n", a[i], $NF; break }
}' /home/chris/myprojects/language-tools/English.wli