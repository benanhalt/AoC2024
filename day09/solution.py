import fileinput

disk_map = next(fileinput.input())

def part1():
    file_table = {}
    free_list = []
    ptr = 0

    for i, bs in enumerate(disk_map):
        block_size = int(bs)
        if block_size < 1:
            continue
        if i % 2 == 0:
            # file block
            file_table[i//2] = [(ptr, ptr + block_size)]
        else:
            # free block
            free_list.append((ptr, ptr + block_size))
        ptr += block_size

    file = i // 2
    # compactify
    while file >= 0:
        file_blocks = file_table[file]
        while True:
            file_block_start, file_block_end = file_blocks[-1]
            file_block_size = file_block_end - file_block_start

            free_start, free_end = free_list[0]
            free_size = free_end - free_start

            if free_start >= file_block_end:
                break 

            file_blocks = file_blocks[:-1]

            if file_block_size < free_size:
                file_blocks = [(free_start, free_start + file_block_size)] + file_blocks
                free_list = [(free_start + file_block_size, free_end)] + free_list[1:] + [(file_block_start, file_block_end)]
                break # done with file

            elif file_block_size == free_size:
                file_blocks = [free_list[0]] + file_blocks
                free_list = free_list[1:]
                break # done with file

            elif file_block_size > free_size:
                file_blocks = [free_list[0]] + file_blocks + [(file_block_start, file_block_end - free_size)]
                free_list = free_list[1:]

            else:
                assert False, "trichotomy"

        file_table[file] = file_blocks
        file -= 1

    # checksum

    checksum = sum(
        file * pos
        for file, blocks in file_table.items()
        for block in blocks
        for pos in range(*block)
    )
    print(checksum)

def part2():
    file_table = {}
    free_list = []
    ptr = 0

    for i, bs in enumerate(disk_map):
        block_size = int(bs)
        if block_size < 1:
            continue
        if i % 2 == 0:
            # file block
            file_table[i//2] = [ptr, ptr + block_size]
        else:
            # free block
            free_list.append((ptr, ptr + block_size))
        ptr += block_size

    file = i // 2
    # compactify
    while file >= 0:
        file_start, file_end = file_table[file]
        file_size = file_end - file_start

        for i, block in enumerate(free_list):
            free_start, free_end = block
            free_size = free_end - free_start

            if free_start >= file_start:
                continue

            if file_size < free_size:
                file_table[file] = (free_start, free_start + file_size)
                free_list.remove(block)
                free_list.append((free_start + file_size, free_end))
                free_list.append((file_start, file_end))
                free_list.sort()
                break
            elif file_size == free_size:
                file_table[file] = (free_start, free_start + file_size)
                free_list.remove(block)
                free_list.append((file_start, file_end))
                free_list.sort()
                break
            else:
                continue

        l = [free_list[0]]
        for block in free_list[1:]:
            a, b = l[-1]
            if l[-1][1] == block[0]:
                l = l[:-1] + [(a, block[1])]
            else:
                l.append(block)

        free_list = l
        file -= 1

    # checksum

    checksum = sum(
        file * pos
        for file, block in file_table.items()
        for pos in range(*block)
    )
    print(checksum)


part1()
part2()