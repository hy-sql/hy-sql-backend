const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe('USER STORY TESTS: large summary queries', () => {
    let stateService

    beforeEach(() => {
        const state = new State(new Map())
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, tuote_id INTEGER, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (1, 'porkkana', 7);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (1, 'banaani', 7);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (1, 'banaani', 7);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (2, 'banaani', 4);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (2, 'retiisi', 7);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (5, 'ol', 2);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (6, 'bucket', 0);",
            "INSERT INTO Tuotteet (tuote_id, nimi, hinta) VALUES (7, 'ol', 1);",
        ]

        const splitCommandArray = commands.map((input) =>
            splitCommandIntoArray(input)
        )

        const parsedCommands = splitCommandArray.map((c) =>
            commandService.parseCommand(c)
        )

        parsedCommands.forEach((c) => stateService.updateState(c))
    })

    test('As a user I want to be able to use GROUP BY', () => {
        const expectedRows = [
            {
                tuote_id: 1,
                'COUNT(*)': 3,
            },
            {
                tuote_id: 2,
                'COUNT(*)': 2,
            },
        ]
        const selectParser =
            'SELECT tuote_id, COUNT(*) FROM Tuotteet WHERE hinta >= 3 GROUP BY tuote_id ORDER BY tuote_id;'
        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })

    test('As a user I want to be able to use COUNT, AVG, MIN and MAX', () => {
        const expectedRows = [
            {
                tuote_id: 1,
                'COUNT(*)': 3,
                'AVG(hinta)': 7,
                'MAX(hinta)': 7,
                'MIN(hinta)': 7,
            },
            {
                tuote_id: 2,
                'COUNT(*)': 2,
                'AVG(hinta)': 5.5,
                'MAX(hinta)': 7,
                'MIN(hinta)': 4,
            },
            {
                tuote_id: 5,
                'COUNT(*)': 1,
                'AVG(hinta)': 2,
                'MAX(hinta)': 2,
                'MIN(hinta)': 2,
            },
        ]
        const selectParser =
            'SELECT tuote_id, COUNT(*), AVG(hinta), MAX(hinta), MIN(hinta) FROM Tuotteet WHERE hinta >= 2 GROUP BY tuote_id  ORDER BY tuote_id;'
        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })

    describe('As a user I want to be able to execute complicated summary queries for tables', () => {
        test('Several columns, functions, aritmetic and AND+OR operators', () => {
            const expectedRows = [
                {
                    nimi: 'ol',
                    hinta: 2,
                    '5*hinta+3': 13,
                    'LENGTH(nimi)': 2,
                },
                {
                    nimi: 'bucket',
                    hinta: 0,
                    '5*hinta+3': 3,
                    'LENGTH(nimi)': 6,
                },
                {
                    nimi: 'banaani',
                    hinta: 7,
                    '5*hinta+3': 38,
                    'LENGTH(nimi)': 7,
                },
                {
                    nimi: 'banaani',
                    hinta: 7,
                    '5*hinta+3': 38,
                    'LENGTH(nimi)': 7,
                },
                {
                    nimi: 'retiisi',
                    hinta: 7,
                    '5*hinta+3': 38,
                    'LENGTH(nimi)': 7,
                },
            ]
            const selectParser =
                "SELECT nimi, hinta, 5*hinta+3, LENGTH(nimi) FROM Tuotteet WHERE hinta=LENGTH(nimi) OR (hinta+1=5 AND nimi<>'banaani') OR (hinta=2*hinta) ORDER BY LENGTH(nimi), hinta;"
            const commandArray = splitCommandIntoArray(selectParser)
            const parsedCommand = commandService.parseCommand(commandArray)

            const result = stateService.updateState(parsedCommand)
            expect(result.rows).toEqual(expectedRows)
        })

        test('Summary: Count products with same name and count the sum of their prices', () => {
            const expectedRows = [
                {
                    nimi: 'banaani',
                    'SUM(hinta)': 18,
                    'COUNT(*)': 3,
                },
                {
                    nimi: 'ol',
                    'SUM(hinta)': 3,
                    'COUNT(*)': 2,
                },
                {
                    nimi: 'bucket',
                    'SUM(hinta)': 0,
                    'COUNT(*)': 1,
                },
                {
                    nimi: 'porkkana',
                    'SUM(hinta)': 7,
                    'COUNT(*)': 1,
                },
                {
                    nimi: 'retiisi',
                    'SUM(hinta)': 7,
                    'COUNT(*)': 1,
                },
            ]
            const selectParser =
                'SELECT nimi, SUM(hinta), COUNT(*) FROM Tuotteet GROUP BY nimi ORDER BY COUNT(*) DESC;'
            const commandArray = splitCommandIntoArray(selectParser)
            const parsedCommand = commandService.parseCommand(commandArray)

            const result = stateService.updateState(parsedCommand)
            expect(result.rows).toEqual(expectedRows)
        })

        test('Summary: Distinct name-price pairs with several conditions', () => {
            const expectedRows = [
                {
                    nimi: 'banaani',
                    hinta: 7,
                },
                {
                    nimi: 'retiisi',
                    hinta: 7,
                },
                {
                    nimi: 'ol',
                    hinta: 2,
                },
                {
                    nimi: 'bucket',
                    hinta: 0,
                },
            ]
            const selectParser =
                "SELECT DISTINCT nimi, hinta FROM Tuotteet WHERE hinta=LENGTH(nimi) OR (hinta+1=5 AND nimi<>'banaani') OR (hinta=2*hinta) ORDER BY hinta DESC;"
            const commandArray = splitCommandIntoArray(selectParser)
            const parsedCommand = commandService.parseCommand(commandArray)

            const result = stateService.updateState(parsedCommand)
            expect(result.rows).toEqual(expectedRows)
        })
    })

    /* TODO WHEN HAVING-FEATURE READY*/
    test.skip('As a user I want to be able to use HAVING', () => {
        const expectedRows = [
            {
                tuote_id: 1,
                'COUNT(*)': 3,
            },
            {
                tuote_id: 2,
                'COUNT(*)': 2,
            },
        ]
        const selectParser =
            'SELECT tuote_id, COUNT(*) FROM Tuotteet WHERE hinta >= 3 GROUP BY tuote_id HAVING COUNT(*) >= 2 ORDER BY tuote_id;'
        const commandArray = splitCommandIntoArray(selectParser)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand)
        expect(result.rows).toEqual(expectedRows)
    })
})
