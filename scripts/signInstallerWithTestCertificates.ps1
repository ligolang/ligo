param([switch]$Elevated, $Path, $Password)

function Test-Admin {
    $currentUser = New-Object Security.Principal.WindowsPrincipal $([Security.Principal.WindowsIdentity]::GetCurrent())
    $currentUser.IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}

if ((Test-Admin) -eq $false)  {
    if ($elevated) {
        # tried to elevate, did not work, aborting
	echo "Could not elevate"
	exit 1
    } else {
        Start-Process powershell.exe -Verb RunAs -ArgumentList ("-noprofile -file {0} -Password ${env:TEST_CERTIFICATE_PASSWORD} -Path $(Resolve-Path $Path) -elevated" -f ($myinvocation.MyCommand.Definition))
    }
    exit
}


$TestCertificatePassword = "default"
if ($env:TEST_CERTIFICATE_PASSWORD) {
    $TestCertificatePassword = $env:TEST_CERTIFICATE_PASSWORD
}
if ($Password) {
    $TestCertificatePassword = $Password
}

$cert = New-SelfSignedCertificate -DnsName ligolang -CertStoreLocation cert:\LocalMachine\My -type CodeSigning
$pwd = ConvertTo-SecureString -String $TestCertificatePassword -Force -AsPlainText
Export-PfxCertificate -cert $cert -FilePath ligolang-certs.pfx -Password $pwd
$env:PATH = $env:PATH + ';C:\Program Files (x86)\Windows Kits\10\App Certification Kit;C:\Program Files (x86)\Windows Kits\10\bin\x64'
signtool.exe sign /a /f ligolang-certs.pfx /p $TestCertificatePassword /fd SHA256 $Path

if (!$?) {
    echo "Could not sign ${Path}"
}
