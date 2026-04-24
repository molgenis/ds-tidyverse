## Version 1.2.1
* feat: Provide option to specify permitted functions via options() (#89)           
* feat: Allow as.Date in mutate (#94)                                               
* feat: Add % to encoding dictionary (#99)                                 
* fix: Regex false-positive on function names like as.numeric( (#97)  

## Version 1.0.4
* Introduced stricter privacy checks to block potential inference attacks

## Version 1.0.0

* Initial release of the package with the following functions included:
`select`, `rename`, `mutate`, `if_else`, `case_when`, `bind_cols`, `bind_rows`, `filter`, `slice`, 
`arrange`, `group_by`, `ungroup`, `group_keys`
