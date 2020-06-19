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
                { name: 'id', type: 'INTEGER', primaryKey: true },
                { name: 'nimi', type: 'TEXT', primaryKey: false },
                { name: 'hinta', type: 'INTEGER', primaryKey: false },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }
        stateService.createTable(command)

        const table = state.getTableByName(command.tableName)
        expect(table.name).toBe('Tuotteet')
        expect(table.columns).toBe(command.columns)
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
                { name: 'id', type: 'INTEGER', primaryKey: true },
                { name: 'nimi', type: 'TEXT', primaryKey: false },
                { name: 'hinta', type: 'INTEGER', primaryKey: false },
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
                { name: 'id', type: 'INTEGER', primaryKey: true },
                { name: 'nimi', type: 'TEXT', primaryKey: false },
                { name: 'nimi', type: 'INTEGER', primaryKey: false },
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
                { name: 'id', type: 'INTEGER', primaryKey: true },
                { name: 'nimi', type: 'TEXT', primaryKey: false },
                { name: 'hinta', type: 'INTEGER', primaryKey: false },
            ],
            closingBracket: ')',
            finalSemicolon: ';',
        }
        stateService.createTable(createCommand)
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
        stateService.insertIntoTable(insertCommand)
        const rows = state.getTableByName(insertCommand.tableName).rows
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
                { name: 'id', type: 'INTEGER', primaryKey: true },
                { name: 'nimi', type: 'TEXT', primaryKey: false },
                { name: 'hinta', type: 'INTEGER', primaryKey: false },
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
