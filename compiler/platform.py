import platform

platform_str = platform.system()

MacOS = platform_str == 'Darwin'
Windows = platform_str == 'Windows'
Linux = platform_str == 'Linux'
