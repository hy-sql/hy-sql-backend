const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')

describe('createTable()', () => {
    test('creates new table to list', () => {
        const initArray = []
        const state = new State(initArray)
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
        expect(state.tables[0].name).toBe('Tuotteet')
    })

    test('returns error when table already exists', () => {
        const initTables = [
            {
                name: 'Tuotteet',
                columns: [],
                rows: [],
            },
        ]
        const state = new State(initTables)
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
        const result = stateService.createTable(command)
        expect(result.error).toBe('Table Tuotteet already exists')
    })

    test('returns error when trying to create duplicate columns', () => {
        const initTables = []
        const state = new State(initTables)
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
        const result = stateService.createTable(command)
        expect(result.error.length).toBe(1)
        expect(result.error[0]).toBe('duplicate column nimi: nimi')
    })
})

describe('insertIntoTable()', () => {
    test('returns error if table does not exist', () => {
        const initTables = []
        const state = new State(initTables)
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
        const result = stateService.insertIntoTable(insertCommand)
        expect(result.error).toBe('No such table Tuotteet')
    })

    test('creates new row succesfully', () => {
        const initArray = []
        const state = new State(initArray)
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
        const rows = state.tables[0].rows
        expect(rows[0].id).toBe(1)
        expect(rows[0].nimi).toBe('tuote')
        expect(rows[0].hinta).toBe(10)
    })

    test('returns error when trying to insert wrong datatype to column', () => {
        const initArray = []
        const state = new State(initArray)
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
        const splitCommand = insertCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(splitCommand)

        const result = stateService.insertIntoTable(parsedCommand.value)
        expect(result.error).toBe(
            'Wrong datatype: expected TEXT but was INTEGER'
        )
    })
})

describe('selectAllFromTable()', () => {
    test('returns error when table does not exist', () => {
        const initTables = []
        const state = new State(initTables)
        const stateService = new StateService(state)
        const selectCommand = {
            name: 'SELECT *',
            tableName: 'Tuotteet',
        }
        const result = stateService.selectAllFromTable(selectCommand)
        expect(result.error).toBe('No such table Tuotteet')
    })

    test('returns all the rows from table', () => {
        const initArray = []
        const state = new State(initArray)
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
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);'
        const splitCommand = insertCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(splitCommand)

        stateService.insertIntoTable(parsedCommand.value)
        const selectCommand = {
            name: 'SELECT *',
            tableName: 'Tuotteet',
        }
        const result = stateService.selectAllFromTable(selectCommand)
        expect(result.rows.length).toBe(1)
    })
})

describe('selectAllFromTable() with ORDER BY -command', () => {
    test('returns rows from table in ascending order', () => {
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)

        const expectedRows = [
            {
                id: 3,
                nimi: 'nauris',
                hinta: 4,
            },
            {
                id: 5,
                nimi: 'selleri',
                hinta: 4,
            },
            {
                id: 2,
                nimi: 'porkkana',
                hinta: 5,
            },
            {
                id: 1,
                nimi: 'retiisi',
                hinta: 7,
            },
            {
                id: 4,
                nimi: 'lanttu',
                hinta: 8,
            },
        ]

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            "INSERT INTO Tuotteet (nimi, hinta) VALUES ('retiisi', 7);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('porkkana',5);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('nauris',4);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('lanttu',8);",
            "INSERT INTO Tuotteet (nimi,hinta) VALUES ('selleri',4);",
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

        const selectAllOrderByCommand = 'SELECT * FROM Tuotteet ORDER BY hinta;'

        const splitSelectAllOrderByCommand = selectAllOrderByCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)

        const parsedSelectAllOrderByCommand = commandService.parseCommand(
            splitSelectAllOrderByCommand
        )

        const result = stateService.selectAllFromTable(
            parsedSelectAllOrderByCommand.value
        )

        expect(result.rows).toEqual(expectedRows)
    })
})

describe('selectFromTable()', () => {
    test('returns error when table does not exist', () => {
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)

        const command = {
            name: 'SELECT',
            tableName: 'products',
        }

        const result = stateService.selectColumnsFromTable(command)
        expect(result.error).toBe('No such table products')
    })

    test('returns column nimi for SELECT nimi FROM Tuotteet;', () => {
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);',
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

        const selectCommand = 'SELECT nimi from Tuotteet;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectColumnsFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT nimi FROM Tuotteet -query was executed successfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0]['nimi']).toBe('tuote')
    })
})

describe('selectAllFromTable() with command.where', () => {
    let stateService

    beforeEach(() => {
        const initArray = []
        const state = new State(initArray)
        stateService = new StateService(state)

        const commands = [
            'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (tuote, 10);',
            'INSERT INTO Tuotteet (nimi, hinta) VALUES (testituote, 20);',
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

    test('returns filtered rows when where is defined', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta=10;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta=10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
    })

    test('returns filtered rows when where-command contains >', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta>10;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta>10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('testituote')
    })

    test('returns filtered rows when where-command contains >=', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta>=20;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta>=20 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('testituote')
    })

    test('returns filtered rows when where-command contains <', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta<20;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta<20 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
    })

    test('returns filtered rows when where-command contains <=', () => {
        const selectCommand = 'SELECT * from Tuotteet WHERE hinta<=10;'
        const commandArray = selectCommand
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
        const parsedCommand = commandService.parseCommand(commandArray)

        const result = stateService.selectAllFromTable(parsedCommand.value)
        expect(result.result).toBe(
            'SELECT * FROM Tuotteet WHERE hinta<=10 -query executed succesfully'
        )
        expect(result.rows.length).toBe(1)
        expect(result.rows[0].nimi).toBe('tuote')
    })
})
