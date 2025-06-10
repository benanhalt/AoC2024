import fileinput

cache = {}

def solve(stone: int, steps: int) -> int:
    if (stone, steps) in cache:
        return cache[(stone, steps)]
    
    if steps == 0:
        result = 1
    
    elif stone == 0:
        result = solve(1, steps - 1)

    elif len(str(stone)) % 2 == 0:
        stone_s = str(stone)
        left = int(stone_s[:len(stone_s)//2])
        right = int(stone_s[len(stone_s)//2:])
        result = solve(left, steps - 1) + solve(right, steps - 1)
    
    else:
        result = solve(2024 * stone, steps - 1)

    cache[(stone, steps)] = result
    return result

stones = list(map(int, next(fileinput.input()).split()))

print(sum(solve(stone, 25) for stone in stones))
print(sum(solve(stone, 75) for stone in stones))
