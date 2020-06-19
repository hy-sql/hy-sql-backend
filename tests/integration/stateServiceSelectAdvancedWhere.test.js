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
            "INSERT INTO Tuotteet (nimi,hinta,lkm) VALUES ('maito', 6, 70);",
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
        'SELECT hinta, nimi FROM Tuotteet WHERE LENGTH(nimi)=5 OR (LENGTH(nimi)<7 AND hinta=4);',
        'SELECT COUNT(nimi) FROM Tuotteet WHERE hinta<=5;',
        "SELECT MAX(hinta) FROM Tuotteet WHERE nimi<>'lanttu';",
        'SELECT MAX(nimi) FROM Tuotteet WHERE hinta>6;',
        'SELECT MIN(lkm) FROM Tuotteet WHERE hinta<=6;',
        'SELECT MIN(nimi) FROM Tuotteet WHERE lkm<60 AND lkm>=30;',
        'SELECT SUM(hinta) FROM Tuotteet WHERE lkm=70;',
        'SELECT SUM(nimi) FROM Tuotteet WHERE lkm>40;',
        "SELECT AVG(hinta) FROM Tuotteet WHERE nimi='selleri';",
        'SELECT AVG(nimi) FROM Tuotteet WHERE hinta<6;',
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

        const commandArray = splitCommandIntoArray(queries[0])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[1]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
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
                nimi: 'porkkana',
            },
        ]

        const commandArray = splitCommandIntoArray(queries[2])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[3]}`, () => {
        const commandArray = splitCommandIntoArray(queries[3])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual([{ 'COUNT(*)': 2 }])
    })

    test(`returns expected rows for: ${queries[4]}`, () => {
        const commandArray = splitCommandIntoArray(queries[4])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual([{ 'COUNT(*)': 6 }])
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

        const commandArray = splitCommandIntoArray(queries[5])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[6]}`, () => {
        const expectedRows = [
            {
                nimi: 'porkkana',
            },
        ]

        const commandArray = splitCommandIntoArray(queries[6])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[7]}`, () => {
        const expectedRows = [
            {
                hinta: 4,
                nimi: 'selleri',
            },
        ]

        const commandArray = splitCommandIntoArray(queries[7])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

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

        const commandArray = splitCommandIntoArray(queries[8])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

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

        const commandArray = splitCommandIntoArray(queries[9])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

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

        const commandArray = splitCommandIntoArray(queries[10])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[11]}`, () => {
        const commandArray = splitCommandIntoArray(queries[11])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual([])
    })

    test(`returns expected rows for: ${queries[12]}`, () => {
        const expectedRows = [
            {
                hinta: 6,
                nimi: 'selleri',
            },
        ]

        const commandArray = splitCommandIntoArray(queries[12])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[13]}`, () => {
        const commandArray = splitCommandIntoArray(queries[13])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual([])
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

        const commandArray = splitCommandIntoArray(queries[14])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual(expectedRows)
    })

    test(`returns expected rows for: ${queries[15]}`, () => {
        const commandArray = splitCommandIntoArray(queries[15])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

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

    test(`returns expected rows for: ${queries[16]}`, () => {
        const commandArray = splitCommandIntoArray(queries[16])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

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

    test(`returns expected rows for: ${queries[17]}`, () => {
        const commandArray = splitCommandIntoArray(queries[17])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

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

    test(`returns expected rows for: ${queries[18]}`, () => {
        const commandArray = splitCommandIntoArray(queries[18])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual([])
    })

    test(`returns expected rows for: ${queries[19]}`, () => {
        const commandArray = splitCommandIntoArray(queries[19])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

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

    test(`returns expected rows for: ${queries[20]}`, () => {
        const commandArray = splitCommandIntoArray(queries[20])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)

        expect(result.rows).toEqual([{ 'COUNT(nimi)': 3 }])
    })

    test(`returns expected rows for: ${queries[21]}`, () => {
        const commandArray = splitCommandIntoArray(queries[21])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'MAX(hinta)': 7 }])
    })

    test(`returns expected rows for: ${queries[22]}`, () => {
        const commandArray = splitCommandIntoArray(queries[22])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'MAX(nimi)': 'retiisi' }])
    })

    test(`returns expected rows for: ${queries[23]}`, () => {
        const commandArray = splitCommandIntoArray(queries[23])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'MIN(lkm)': 30 }])
    })

    test(`returns expected rows for: ${queries[24]}`, () => {
        const commandArray = splitCommandIntoArray(queries[24])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'MIN(nimi)': 'nauris' }])
    })

    test(`returns expected rows for: ${queries[25]}`, () => {
        const commandArray = splitCommandIntoArray(queries[25])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'SUM(hinta)': 12 }])
    })

    test(`returns expected rows for: ${queries[26]}`, () => {
        const commandArray = splitCommandIntoArray(queries[26])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'SUM(nimi)': 0 }])
    })

    test(`returns expected rows for: ${queries[27]}`, () => {
        const commandArray = splitCommandIntoArray(queries[27])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'AVG(hinta)': 5 }])
    })

    test(`returns expected rows for: ${queries[28]}`, () => {
        const commandArray = splitCommandIntoArray(queries[28])
        const parsedCommand = commandService.parseCommand(commandArray)
        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual([{ 'AVG(nimi)': 0 }])
    })
})
