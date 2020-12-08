import platform

platform_str = platform.system()

MacOS = platform_str == 'Darwin'
Windows = platform_str == 'Windows'
Linux = platform_str == 'Linux'

readChecksum = None

if MacOS:
    from macholib.MachO       import MachO
    from macholib.SymbolTable import SymbolTable
    
    def readChecksum__MachO(importInfo):
        if not importInfo.objectFile:
            return None
        
        name = importInfo.name
        macho = MachO(importInfo.objectFile)
        symbols = SymbolTable(macho)
        checksumSymbol = 'M{}{}$checksum'.format(len(name.content), name.content).encode('utf-8')
        nlistFiltered = [i[0] for i in symbols.nlists if i[1] == checksumSymbol]
        nlist = nlistFiltered[0] if nlistFiltered else None
        if nlist:
            section = macho.headers[0].commands[0][2][nlist.n_sect-1]
            sectionData = section.section_data
            offset = nlist.n_value - section.addr
            checksum = int.from_bytes(sectionData[offset:offset+4], "little")
            return checksum
        return None
    
    readChecksum = readChecksum__MachO
    
