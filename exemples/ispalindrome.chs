// Checks if a number is palindrome
Palindrome := 101;

x := Palindrome;

reverse := 0;
remainder := 0;
temp := Palindrome;

(< x 0) if { "False\n" print } else {
    while (!= temp 0) {
        temp 10 mod := remainder
        (+ (* reverse 10) remainder) := reverse
        temp 10 / := temp
    }
    (= reverse x) if { "True\n" print } else { "False\n" print }
}