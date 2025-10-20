# Comprehensive test script for Ratatouille parser
# PowerShell version for Windows

Write-Host "================================" -ForegroundColor Cyan
Write-Host "RATATOUILLE PARSER TEST SUITE" -ForegroundColor Cyan
Write-Host "================================" -ForegroundColor Cyan
Write-Host ""

$total = 0
$passed = 0
$failed = 0

function Test-RatFile {
    param($file)

    $global:total++
    $name = Split-Path $file -Leaf

    Write-Host "Testing $name... " -NoNewline

    $result = & .\glados.exe $file 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "[PASS]" -ForegroundColor Green
        $global:passed++
        return $true
    } else {
        Write-Host "[FAIL]" -ForegroundColor Red
        $global:failed++
        Write-Host "  Error: " -NoNewline
        $result | Select-String "Parse Error" | ForEach-Object { Write-Host $_ -ForegroundColor Red }
        return $false
    }
}

Write-Host "=== BASIC EXAMPLES ===" -ForegroundColor Blue
Test-RatFile "examples\basics\helloWorld.rat"
Test-RatFile "examples\basics\counter.rat"
Write-Host ""

Write-Host "=== ADVANCED EXAMPLES ===" -ForegroundColor Blue
Test-RatFile "examples\advanced\errorHandling.rat"
Test-RatFile "examples\advanced\asynchroneCalc.rat"
Test-RatFile "examples\advanced\recursiveCounter.rat"
Test-RatFile "examples\advanced\triangularComm.rat"
Write-Host ""

Write-Host "================================" -ForegroundColor Cyan
Write-Host "TEST SUMMARY" -ForegroundColor Cyan
Write-Host "================================" -ForegroundColor Cyan
Write-Host "Total tests:  $total"
Write-Host "Passed:       $passed" -ForegroundColor Green
Write-Host "Failed:       $failed" -ForegroundColor $(if ($failed -eq 0) { "Green" } else { "Red" })
Write-Host ""

if ($failed -eq 0) {
    Write-Host "All tests passed!" -ForegroundColor Green
    exit 0
} else {
    Write-Host "Some tests failed!" -ForegroundColor Red
    exit 1
}
