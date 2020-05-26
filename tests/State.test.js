const State = require('../models/State')
const StateService = require('../services/StateService')

test('CREATE TABLE creates new table to list', () => {
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

test('CREATE TABLE returns error when table already exists', () => {
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

test('CREATE TABLE returns error when trying to create duplicate columns', () => {
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

test('INSERT INTO returns error if table does not exist', () => {
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

test('SELECT * FROM table that does not exist returns error', () => {
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

test('INSERT INTO creates row and updates id', () => {
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
    console.log(state.tables[0])
    const rows = state.tables[0].rows
    console.log(rows)
    expect(rows[0].id).toBe(1)
    expect(rows[0].nimi).toBe('tuote')
    expect(rows[0].hinta).toBe(10)
})

test('SELECT * FROM returns rows from table', () => {
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
        columns: ['nimi', 'hinta'],
        values: ['tuote', 10],
    }
    stateService.insertIntoTable(insertCommand)
    const selectCommand = {
        name: 'SELECT *',
        tableName: 'Tuotteet',
    }
    const result = stateService.selectAllFromTable(selectCommand)
    expect(result.rows.length).toBe(1)
})
