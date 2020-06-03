const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')

describe('selectAdvanced()', () => {
    let stateService
    beforeEach(() => {
        const initArray = []
        const state = new State(initArray)
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
            input
                .trim()
                .replace(/\s\s+/g, ' ')
                .replace(/\s+,/g, ',')
                .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
                .filter(Boolean)
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

        const selectCommand = 'SELECT 5*hinta-3 FROM Tuotteet;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

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

        const selectCommand = 'SELECT LENGTH(nimi) FROM Tuotteet;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

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

        const selectCommand = 'SELECT 5+hinta*4, LENGTH(nimi) FROM Tuotteet;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

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

        const selectCommand = 'SELECT LENGTH(nimi), 5+hinta*4 FROM Tuotteet;'

        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.updateState(parsedCommand.value)
        expect(result.rows).toEqual(expectedRows)
    })
})
