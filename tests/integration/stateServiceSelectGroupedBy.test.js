const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe('groupRowsBy()', () => {
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
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('selleri', 4, 70);",
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('maito', 6, 70);",
            "INSERT INTO Tuotteet (nimi,hinta, lkm) VALUES ('olut',4, 50);",
            "INSERT INTO Tuotteet (nimi,hinta, lkm) VALUES ('olut',4, 70);",
            "INSERT INTO Tuotteet (nimi,hinta, lkm) VALUES ('olut',5, 40);",
            "INSERT INTO Tuotteet (nimi,hinta, lkm) VALUES ('olut',5, 40);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )
        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )
        parsedCommands.forEach((c) => stateService.updateState(c))
    })

    const queries = [
        'SELECT nimi, hinta FROM Tuotteet GROUP BY nimi;',
        'SELECT nimi, hinta FROM Tuotteet WHERE hinta=4 GROUP BY nimi;',
        "SELECT nimi, COUNT(nimi) FROM Tuotteet WHERE nimi='olut' GROUP BY COUNT(nimi);",
        'SELECT nimi, SUM(lkm) FROM Tuotteet GROUP BY nimi;',
        'SELECT nimi, hinta, SUM(lkm), COUNT(*) FROM Tuotteet GROUP BY nimi, hinta ORDER BY hinta, nimi;',
    ]

    test(`returns expected rows for: ${queries[0]}`, () => {
        const expectedRows = [
            {
                nimi: 'lanttu',
                hinta: 8,
            },
            {
                nimi: 'maito',
                hinta: 6,
            },
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'olut',
                hinta: 4,
            },
            {
                nimi: 'olut',
                hinta: 5,
            },
            {
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                nimi: 'selleri',
                hinta: 4,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[0])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[1]}`, () => {
        const expectedRows = [
            {
                nimi: 'nauris',
                hinta: 4,
            },
            {
                nimi: 'olut',
                hinta: 4,
            },
            {
                nimi: 'selleri',
                hinta: 4,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[1])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[2]}`, () => {
        const expectedRows = [
            {
                nimi: 'olut',
                'COUNT(nimi)': 4,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[2])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[3]}`, () => {
        const expectedRows = [
            {
                nimi: 'lanttu',
                'SUM(lkm)': 20,
            },
            {
                nimi: 'maito',
                'SUM(lkm)': 70,
            },
            {
                nimi: 'nauris',
                'SUM(lkm)': 40,
            },
            {
                nimi: 'olut',
                'SUM(lkm)': 200,
            },
            {
                nimi: 'porkkana',
                'SUM(lkm)': 40,
            },
            {
                nimi: 'retiisi',
                'SUM(lkm)': 20,
            },
            {
                nimi: 'selleri',
                'SUM(lkm)': 100,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[3])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[4]}`, () => {
        const expectedRows = [
            {
                nimi: 'nauris',
                hinta: 4,
                'SUM(lkm)': 40,
                'COUNT(*)': 1,
            },
            {
                nimi: 'olut',
                hinta: 4,
                'SUM(lkm)': 120,
                'COUNT(*)': 2,
            },
            {
                nimi: 'selleri',
                hinta: 4,
                'SUM(lkm)': 100,
                'COUNT(*)': 2,
            },
            {
                nimi: 'olut',
                hinta: 5,
                'SUM(lkm)': 80,
                'COUNT(*)': 2,
            },
            {
                nimi: 'porkkana',
                hinta: 5,
                'SUM(lkm)': 40,
                'COUNT(*)': 1,
            },
            {
                nimi: 'maito',
                hinta: 6,
                'SUM(lkm)': 70,
                'COUNT(*)': 1,
            },
            {
                nimi: 'retiisi',
                hinta: 7,
                'SUM(lkm)': 20,
                'COUNT(*)': 1,
            },
            {
                nimi: 'lanttu',
                hinta: 8,
                'SUM(lkm)': 20,
                'COUNT(*)': 1,
            },
        ]

        const commandArray = splitCommandIntoArray(queries[4])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })
})
