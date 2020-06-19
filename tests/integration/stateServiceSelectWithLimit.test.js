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
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('porkkana', 5);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris', 4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('lanttu', 8);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri', 6);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('maito', 6);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )
        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )
        parsedCommands.forEach((c) => stateService.updateState(c.value))
    })

    const queries = [
        'SELECT nimi, hinta FROM Tuotteet LIMIT 2;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 2 + 1;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 2*2;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 20;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 2;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 30;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 2 OFFSET 1 + 2;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 3 OFFSET 3*1;',
        'SELECT nimi, hinta FROM Tuotteet WHERE hinta<8 LIMIT 2 OFFSET 1;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT -3;',
        'SELECT nimi, hinta FROM Tuotteet LIMIT 1 OFFSET -2;',
        'SELECT nimi, hinta FROM Tuotteet GROUP BY nimi LIMIT 2 OFFSET 1;',
        'SELECT nimi, hinta FROM Tuotteet ORDER BY hinta LIMIT 2 OFFSET 1;',
    ]

    test(`returns expected rows for: ${queries[0]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[0])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[1]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'lanttu',
                hinta: 8,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[1])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[2]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'lanttu',
                hinta: 8,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[2])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[3]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'lanttu',
                hinta: 8,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
            {
                nimi: 'maito',
                hinta: 6,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[3])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[4]}`, () => {
        const expectedRows = [
            {
                nimi: 'lanttu',
                hinta: 8,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[4])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected error for: ${queries[5]}`, () => {
        const commandArray = splitCommandIntoArray(queries[5])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toBeUndefined()
        expect(result).toEqual({
            error:
                'No rows left to return. Try changing value given to LIMIT or OFFSET.',
        })
    })

    test(`returns expected rows for: ${queries[6]}`, () => {
        const expectedRows = [
            {
                nimi: 'selleri',
                hinta: 6,
            },
            {
                nimi: 'maito',
                hinta: 6,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[6])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[7]}`, () => {
        const expectedRows = [
            {
                nimi: 'selleri',
                hinta: 6,
            },
            {
                nimi: 'maito',
                hinta: 6,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[7])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[8]}`, () => {
        const expectedRows = [
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[8])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)
        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected error for: ${queries[9]}`, () => {
        const commandArray = splitCommandIntoArray(queries[9])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toBeUndefined()
        expect(result).toEqual({
            error: 'Value given to LIMIT or OFFSET is negative.',
        })
    })

    test(`returns expected error for: ${queries[10]}`, () => {
        const commandArray = splitCommandIntoArray(queries[10])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toBeUndefined()
        expect(result).toEqual({
            error: 'Value given to LIMIT or OFFSET is negative.',
        })
    })

    test(`returns expected rows for: ${queries[11]}`, () => {
        const expectedRows = [
            {
                nimi: 'maito',
                hinta: 6,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[11])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[12]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'selleri',
                hinta: 6,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[12])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.error).toBeUndefined()
        expect(result.rows).toEqual(expectedRows)
    })
})
