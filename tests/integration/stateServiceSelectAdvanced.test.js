const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe('selectFrom()', () => {
    let stateService
    beforeEach(() => {
        const state = new State(new Map())
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER, lkm INTEGER);',
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('retiisi', 7, 20);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('porkkana', 5, 40);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('nauris', 4, 40);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('lanttu', 8, 20);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('selleri', 4, 30);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('selleri', 6, 70);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('null', 6, 70);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c.value))
    })

    test('returns rows asked by select arithmetic expression', () => {
        const expectedRows = [
            {
                '5*hinta-3': 32,
            },
            {
                '5*hinta-3': 22,
            },
            {
                '5*hinta-3': 17,
            },
            {
                '5*hinta-3': 37,
            },
            {
                '5*hinta-3': 17,
            },
            {
                '5*hinta-3': 27,
            },
            {
                '5*hinta-3': 27,
            },
        ]

        const selectParser = 'SELECT 5*hinta-3 FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns rows asked by select function expression', () => {
        const expectedRows = [
            {
                'LENGTH(nimi)': 7,
            },
            {
                'LENGTH(nimi)': 8,
            },
            {
                'LENGTH(nimi)': 6,
            },
            {
                'LENGTH(nimi)': 6,
            },
            {
                'LENGTH(nimi)': 7,
            },
            {
                'LENGTH(nimi)': 7,
            },
            {
                'LENGTH(nimi)': 4,
            },
        ]

        const selectParser = 'SELECT LENGTH(nimi) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns rows asked by select arithmetic and function expression', () => {
        const expectedRows = [
            {
                '5+hinta*4': 33,
                'LENGTH(nimi)': 7,
            },
            {
                '5+hinta*4': 25,
                'LENGTH(nimi)': 8,
            },
            {
                '5+hinta*4': 21,
                'LENGTH(nimi)': 6,
            },
            {
                '5+hinta*4': 37,
                'LENGTH(nimi)': 6,
            },
            {
                '5+hinta*4': 21,
                'LENGTH(nimi)': 7,
            },
            {
                '5+hinta*4': 29,
                'LENGTH(nimi)': 7,
            },
            {
                '5+hinta*4': 29,
                'LENGTH(nimi)': 4,
            },
        ]

        const selectParser = 'SELECT 5+hinta*4, LENGTH(nimi) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns rows asked by select function and arithmetic expression (reversed previous test)', () => {
        const expectedRows = [
            {
                'LENGTH(nimi)': 7,
                '5+hinta*4': 33,
            },
            {
                'LENGTH(nimi)': 8,
                '5+hinta*4': 25,
            },
            {
                'LENGTH(nimi)': 6,
                '5+hinta*4': 21,
            },
            {
                'LENGTH(nimi)': 6,
                '5+hinta*4': 37,
            },
            {
                'LENGTH(nimi)': 7,
                '5+hinta*4': 21,
            },
            {
                'LENGTH(nimi)': 7,
                '5+hinta*4': 29,
            },
            {
                'LENGTH(nimi)': 4,
                '5+hinta*4': 29,
            },
        ]

        const selectParser = 'SELECT LENGTH(nimi), 5+hinta*4 FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns rows asked by select function, arithmetic expression and standart column', () => {
        const expectedRows = [
            {
                'length(nimi)': 7,
                '5+hinta*4': 33,
                lkm: 20,
            },
            {
                'length(nimi)': 8,
                '5+hinta*4': 25,
                lkm: 40,
            },
            {
                'length(nimi)': 6,
                '5+hinta*4': 21,
                lkm: 40,
            },
            {
                'length(nimi)': 6,
                '5+hinta*4': 37,
                lkm: 20,
            },
            {
                'length(nimi)': 7,
                '5+hinta*4': 21,
                lkm: 30,
            },
            {
                'length(nimi)': 7,
                '5+hinta*4': 29,
                lkm: 70,
            },
            {
                'length(nimi)': 4,
                '5+hinta*4': 29,
                lkm: 70,
            },
        ]

        const selectParser =
            'SELECT length(nimi), 5+hinta*4, lkm FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns row asked by COUNT function in select', () => {
        const expectedRows = [
            {
                'COUNT(nimi)': 7,
            },
        ]

        const selectParser = 'SELECT COUNT(nimi) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })

    test('returns row asked by COUNT function in select', () => {
        const expectedRows = [
            {
                'COUNT(*)': 7,
            },
        ]

        const selectParser = 'SELECT COUNT(*) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test('returns row asked by select function expression', () => {
        const expectedRows = [
            {
                "LENGTH('string')": 6,
            },
            {
                "LENGTH('string')": 6,
            },
            {
                "LENGTH('string')": 6,
            },
            {
                "LENGTH('string')": 6,
            },
            {
                "LENGTH('string')": 6,
            },
            {
                "LENGTH('string')": 6,
            },
            {
                "LENGTH('string')": 6,
            },
        ]

        const selectParser = "SELECT LENGTH('string') FROM Tuotteet;"

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    /* FIXME: Create massive regular expression for arithmetic expression field with functions etc...
    test('returns row asked by COUNT function in select with arithmetic expression', () => {
        const expectedRows = [
            {
                'COUNT(*)*2': 14,
            },
        ]

        const selectParser = 'SELECT COUNT(*)*2 FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

     test('returns row asked by select function expression', () => {
        const expectedRows = [
            {
                "LENGTH('string')*2": 12,
            },
            {
                "LENGTH('string')*2": 12,
            },
            {
                "LENGTH('string')*2": 12,
            },
            {
                "LENGTH('string')*2": 12,
            },
            {
                "LENGTH('string')*2": 12,
            },
            {
                "LENGTH('string')*2": 12,
            },
            {
                "LENGTH('string')*2": 12,
            },
        ]

        const selectParser = "SELECT LENGTH('string')*2 FROM Tuotteet;"

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })
    */

    test('returns expected error for LENGTH-function in select', () => {
        const selectParser = 'SELECT LENGTH(nonexistent) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.error).toBeDefined()
        expect(result.error).toBe(
            'Column name given to LENGTH as parameter does not match any existing column'
        )
    })

    test('returns row asked by MAX-function in select', () => {
        const selectParser = 'SELECT MAX(hinta) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'MAX(hinta)': 8 }])
    })

    test('returns row asked by MAX-function in select', () => {
        const selectParser = 'SELECT MAX(nimi) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'MAX(nimi)': 'selleri' }])
    })

    test('returns expected error for MAX-function in select', () => {
        const selectParser = 'SELECT MAX(nonexistent) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.error).toBeDefined()
        expect(result.error).toBe(
            'Parameter given to MAX does not match any existing column'
        )
    })

    test('returns row asked by MIN-function in select', () => {
        const selectParser = 'SELECT MIN(hinta) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'MIN(hinta)': 4 }])
    })

    test('returns row asked by MIN-function in select', () => {
        const selectParser = 'SELECT MIN(nimi) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'MIN(nimi)': 'lanttu' }])
    })

    test('returns expected error for MIN-function in select', () => {
        const selectParser = 'SELECT MIN(nonexistent) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.error).toBeDefined()
        expect(result.error).toBe(
            'Parameter given to MIN does not match any existing column'
        )
    })

    test('returns row asked by SUM-function in select', () => {
        const selectParser = 'SELECT SUM(hinta) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'SUM(hinta)': 40 }])
    })

    test('returns row asked by SUM-function in select', () => {
        const selectParser = 'SELECT SUM(nimi) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'SUM(nimi)': 0 }])
    })

    test('returns expected error for SUM-function in select', () => {
        const selectParser = 'SELECT SUM(nonexistent) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.error).toBeDefined()
        expect(result.error).toBe(
            'Parameter given to SUM does not match any existing column'
        )
    })

    test('returns row asked by AVG-function in select', () => {
        const selectParser = 'SELECT AVG(hinta) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'AVG(hinta)': 5.714285714285714 }])
    })

    test('returns row asked by AVG-function in select', () => {
        const selectParser = 'SELECT AVG(nimi) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual([{ 'AVG(nimi)': 0 }])
    })

    test('returns expected error for AVG-function in select', () => {
        const selectParser = 'SELECT AVG(nonexistent) FROM Tuotteet;'

        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.error).toBeDefined()
        expect(result.error).toBe(
            'Parameter given to AVG does not match any existing column'
        )
    })
})
