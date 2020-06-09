const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')
const cleanCommand = require('../utils/cleanCommand')

describe('selectAdvanced()', () => {
    let stateService
    beforeEach(() => {
        const state = new State([])
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER, lkm INTEGER);',
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('retiisi', 7, 20);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('porkkana', 5, 40);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('nauris', 4, 40);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('lanttu', 8, 20);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('selleri', 4, 30);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('selleri', 6, 70);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('maito', 6, 70);",
        ]

        const splitCommandArray = commands.map((input) => cleanCommand(input))
        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )
        parsedCommands.forEach((c) => stateService.updateState(c.value))
    })

    const queries = [
        'SELECT 5*hinta-3, nimi FROM Tuotteet WHERE hinta=4;',
        'SELECT nimi FROM Tuotteet WHERE hinta=2+3;',
        'SELECT nimi FROM Tuotteet WHERE hinta+2=7;',
        'SELECT COUNT(*) FROM Tuotteet WHERE hinta<5;',
        'SELECT COUNT(*) FROM Tuotteet WHERE hinta<>5;',
        'SELECT LENGTH(nimi), nimi FROM Tuotteet WHERE hinta<=5;',
        'SELECT nimi FROM Tuotteet WHERE LENGTH(nimi)=8;',
        'SELECT hinta, nimi FROM Tuotteet WHERE hinta=4 AND lkm<40;',
        'SELECT hinta, nimi FROM Tuotteet WHERE hinta=5 OR lkm>60;',
        'SELECT hinta, nimi FROM Tuotteet WHERE hinta=5 OR (hinta=4 AND lkm=30);',
        "SELECT hinta, nimi FROM Tuotteet WHERE hinta=5 OR nimi='maito' OR lkm=30;",
        "SELECT hinta, nimi FROM Tuotteet WHERE hinta=5 AND nimi='maito';",
        "SELECT hinta, nimi FROM Tuotteet WHERE hinta<7 AND nimi='selleri' AND lkm=70;",
        "SELECT hinta, nimi FROM Tuotteet WHERE hinta=5 AND nimi='porkkana' AND lkm=30;",
        "SELECT nimi, hinta, LENGTH(nimi) FROM Tuotteet WHERE hinta=LENGTH(nimi) OR (hinta+1=5 AND nimi<>'selleri') OR hinta=2*hinta;",
        'SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)=5 OR (hinta=4 AND LENGTH(nimi)<7);',
        'SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)>=6 AND (hinta=5 OR lkm>60);',
        'SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)=5 OR LENGTH(nimi)>7;',
        'SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)<=5 AND LENGTH(nimi)>=7;',
    ]

    test(`returns expected rows for: ${queries[0]}`, () => {
        const expectedRows = [
            {
                '5*hinta-3': 17,
                nimi: 'nauris',
            },
            {
                '5*hinta-3': 17,
                nimi: 'selleri',
            },
        ]

        const commandArray = cleanCommand(queries[0])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[1]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
            },
        ]

        const commandArray = cleanCommand(queries[1])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[2]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
            },
        ]

        const commandArray = cleanCommand(queries[2])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[3]}`, () => {
        const expectedRows = [
            {
                'COUNT(*)': 2,
            },
        ]

        const commandArray = cleanCommand(queries[3])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[4]}`, () => {
        const expectedRows = [
            {
                'COUNT(*)': 6,
            },
        ]

        const commandArray = cleanCommand(queries[4])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[5]}`, () => {
        const expectedRows = [
            {
                'LENGTH(nimi)': 8,
                nimi: 'porkkana',
            },
            {
                'LENGTH(nimi)': 6,
                nimi: 'nauris',
            },
            {
                'LENGTH(nimi)': 7,
                nimi: 'selleri',
            },
        ]

        const commandArray = cleanCommand(queries[5])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[6]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
            },
        ]

        const commandArray = cleanCommand(queries[6])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[7]}`, () => {
        const expectedRows = [
            {
                hinta: 4,
                nimi: 'selleri',
            },
        ]

        const commandArray = cleanCommand(queries[7])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[8]}`, () => {
        const expectedRows = [
            {
                hinta: 5,
                nimi: 'porkkana',
            },
            {
                hinta: 6,
                nimi: 'selleri',
            },
            {
                hinta: 6,
                nimi: 'maito',
            },
        ]

        const commandArray = cleanCommand(queries[8])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[9]}`, () => {
        const expectedRows = [
            {
                hinta: 5,
                nimi: 'porkkana',
            },
            {
                hinta: 4,
                nimi: 'selleri',
            },
        ]

        const commandArray = cleanCommand(queries[9])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[10]}`, () => {
        const expectedRows = [
            {
                hinta: 5,
                nimi: 'porkkana',
            },
            {
                hinta: 4,
                nimi: 'selleri',
            },
            {
                hinta: 6,
                nimi: 'maito',
            },
        ]

        const commandArray = cleanCommand(queries[10])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[11]}`, () => {
        const expectedRows = []

        const commandArray = cleanCommand(queries[11])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[12]}`, () => {
        const expectedRows = [
            {
                hinta: 6,
                nimi: 'selleri',
            },
        ]

        const commandArray = cleanCommand(queries[12])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[13]}`, () => {
        const expectedRows = []

        const commandArray = cleanCommand(queries[13])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[14]}`, () => {
        const expectedRows = [
            {
                nimi: 'retiisi',
                hinta: 7,
                'LENGTH(nimi)': 7,
            },
            {
                nimi: 'nauris',
                hinta: 4,
                'LENGTH(nimi)': 6,
            },
        ]

        const commandArray = cleanCommand(queries[14])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual(expectedRows)
    })

    /*
    //''SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)=5 OR (hinta=4 AND LENGTH(nimi)<7);''
    test(`returns expected rows for: ${queries[15]}`, () => {
        const commandArray = cleanCommand(queries[15])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toHaveLength(2)
        expect(result.rows).toContainEqual({
            hinta: 6,
            nimi: 'maito',
        })
        expect(result.rows).toContainEqual({
            hinta: 4,
            nimi: 'nauris',
        })
    })



    //'SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)>=6 AND (hinta=5 OR lkm>60);'
    test(`returns expected rows for: ${queries[16]}`, () => {
        const commandArray = cleanCommand(queries[16])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toHaveLength(2)
        expect(result.rows).toContainEqual({
            hinta: 5,
            nimi: 'porkkana',
        })
        expect(result.rows).toContainEqual({
            hinta: 6,
            nimi: 'selleri',
        })
    })

    //'SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)=5 OR LENGTH(nimi)>7;'
    test(`returns expected rows for: ${queries[17]}`, () => {
        const commandArray = cleanCommand(queries[17])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toHaveLength(2)
        expect(result.rows).toContainEqual({
            hinta: 6,
            nimi: 'maito',
        })
        expect(result.rows).toContainEqual({
            hinta: 5,
            nimi: 'porkkana',
        })
    })
    */

    test(`returns expected rows for: ${queries[18]}`, () => {
        const commandArray = cleanCommand(queries[18])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand.value)

        expect(result.rows).toEqual([])
    })
})
