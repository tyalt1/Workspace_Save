$nic = Get-WmiObject Win32_NetworkAdapterConfiguration -Filter "ipenabled = 'true'"

function Pause {

    Write-Output 'Press any key to continue...'
    $HOST.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown") | Out-Null
    $HOST.UI.RawUI.FlushInputBuffer()
}

function Dev-On {

    #Set IP and Subnet mask.
    $nic.EnableStatic("10.0.0.1", "255.0.0.0")
    #Disable Firewall
    Set-NetFirewallProfile -All -Enabled “false”
}

function Dev-Off {

    #Enable Dynamic IP
    $nic.EnableDHCP()
    #Enable Firewall
    Set-NetFirewallProfile -All -Enabled “true”
}

Dev-On
Write-Output 'Philips Developer mode is now on.'
Write-Output 'IP is 10.0.0.1, Subnet Mask is 255.0.0.0, Firewall is off.'

Pause
Dev-Off
Remove-Variable nic