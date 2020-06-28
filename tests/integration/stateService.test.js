const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')
const SQLError = require('../../models/SQLError')

describe('createTable()', () => {
    test('creates new table to list', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)
        const command = {
            name: 'CREATE TABLE',
            tableName: 'Tuotteet',
            openingBracket: '(',
            columns: [
                { name: 'id', type: 'INTEGER', constraints: ['PRIMARY KEY'] },
                { name: 'nimi', type: 'TEXT', constraints: [] },
                { name: 'hinta', type: 'INTEGER', constraints: [] },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }
        stateService.createTable(command)

        const table = state.getTableByName(command.tableName)
        expect(table.name).toBe('Tuotteet')
        expect(table.columns).toStrictEqual(command.columns)
    })

    test('returns error when table already exists', () => {
        const existingTable = {
            name: 'Tuotteet',
            columns: [],
            rows: [],
        }

        const state = new State(new Map())
        const stateService = new StateService(state)

        state.createTable(existingTable)

        const command = {
            name: 'CREATE TABLE',
            tableName: 'Tuotteet',
            openingBracket: '(',
            columns: [
                { name: 'id', type: 'INTEGER', constraints: ['PRIMARY KEY'] },
                { name: 'nimi', type: 'TEXT', constraints: [] },
                { name: 'hinta', type: 'INTEGER', constraints: [] },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }

        expect(() => stateService.createTable(command)).toThrowError(
            new SQLError('Table Tuotteet already exists')
        )
    })

    test('returns error when trying to create duplicate columns', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)
        const command = {
            name: 'CREATE TABLE',
            tableName: 'Tuotteet',
            openingBracket: '(',
            columns: [
                { name: 'id', type: 'INTEGER', constraints: ['PRIMARY KEY'] },
                { name: 'nimi', type: 'TEXT', constraints: [] },
                { name: 'nimi', type: 'INTEGER', constraints: [] },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }

        expect(() => stateService.createTable(command)).toThrowError(
            new SQLError('duplicate column nimi: nimi')
        )
    })
})

describe('insertIntoTable()', () => {
    test('returns error if table does not exist', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)
        const insertCommand = {
            name: 'INSERT INTO',
            tableName: 'Tuotteet',
            columns: [
                {
                    name: 'nimi',
                },
                {
                    name: 'hinta',
                },
            ],
            values: [
                {
                    column: 'nimi',
                    value: 'tuote',
                    type: 'TEXT',
                },
                {
                    column: 'hinta',
                    value: 10,
                    type: 'INTEGER',
                },
            ],
        }

        expect(() => stateService.insertIntoTable(insertCommand)).toThrowError(
            new SQLError('No such table Tuotteet')
        )
    })

    test('creates new row succesfully', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)
        const createCommand = {
            name: 'CREATE TABLE',
            tableName: 'Tuotteet',
            openingBracket: '(',
            columns: [
                { name: 'id', type: 'INTEGER', constraints: ['PRIMARY KEY'] },
                { name: 'nimi', type: 'TEXT', constraints: [] },
                { name: 'hinta', type: 'INTEGER', constraints: [] },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }

        stateService.createTable(createCommand)

        const insertCommand =
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('tuote', 10);"

        const splitInsertCommand = splitCommandIntoArray(insertCommand)

        const parsedCommand = commandService.parseCommand(splitInsertCommand)

        stateService.insertIntoTable(parsedCommand)

        const rows = state.getTableByName(parsedCommand.tableName).rows
        expect(rows[0].id).toBe(1)
        expect(rows[0].nimi).toBe('tuote')
        expect(rows[0].hinta).toBe(10)
    })

    test('returns error when trying to insert wrong datatype to column', () => {
        const state = new State(new Map())
        const stateService = new StateService(state)
        const createCommand = {
            name: 'CREATE TABLE',
            tableName: 'Tuotteet',
            openingBracket: '(',
            columns: [
                { name: 'id', type: 'INTEGER', constraints: ['PRIMARY KEY'] },
                { name: 'nimi', type: 'TEXT', constraints: [] },
                { name: 'hinta', type: 'INTEGER', constraints: [] },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }
        stateService.createTable(createCommand)

        const insertCommand =
            "INSERT INTO Tuotteet (nimi, hinta) VALUES (10, 'tuote' );"
        const splitCommand = splitCommandIntoArray(insertCommand)
        const parsedCommand = commandService.parseCommand(splitCommand)

        expect(() => stateService.insertIntoTable(parsedCommand)).toThrowError(
            new SQLError('Wrong datatype: expected TEXT but was INTEGER')
        )
    })
})

describe('selectDistinct()', () => {
    const state = new State(new Map())
    const stateService = new StateService(state)
    const rows = [
        {
            nimi: 'olut',
            hinta: 3,
        },
        {
            nimi: 'olut',
            hinta: 3,
        },
        {
            nimi: 'olut',
            hinta: 2,
        },
        {
            nimi: 'nauris',
            hinta: 3,
        },
        {
            nimi: 'nauris',
            hinta: 3,
        },
    ]

    test('filters out duplicate rows', () => {
        const expectedRows = [
            {
                nimi: 'olut',
                hinta: 3,
            },
            {
                nimi: 'olut',
                hinta: 2,
            },
            {
                nimi: 'nauris',
                hinta: 3,
            },
        ]
        const result = stateService.selectDistinct(rows)
        expect(result).toEqual(expectedRows)
    })
})
