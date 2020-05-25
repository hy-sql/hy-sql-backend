const State = require('../models/State')

test('CREATE TABLE creates new table to list', () => {
    const initArray = []
    const state = new State(initArray)
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
    state.createTable(command)
    expect(state.tables[0].name).toBe('Tuotteet')
})

test('INSERT INTO creates row and updates id', () => {
    const initArray = []
    const state = new State(initArray)
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
    state.createTable(createCommand)
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
    state.insertIntoTable(insertCommand)
    const rows = state.tables[0].rows
    expect(rows[0].id).toBe(1)
    expect(rows[0].nimi).toBe('tuote')
    expect(rows[0].hinta).toBe(10)
})

test('SELECT * FROM returns rows from table', () => {
    const initArray = []
    const state = new State(initArray)
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
    state.createTable(createCommand)
    const insertCommand = {
        name: 'INSERT INTO',
        tableName: 'Tuotteet',
        columns: ['nimi', 'hinta'],
        values: ['tuote', 10],
    }
    state.insertIntoTable(insertCommand)
    const selectCommand = {
        name: 'SELECT *',
        tableName: 'Tuotteet',
    }
    const result = state.selectAllFromTable(selectCommand)
    expect(result.rows.length).toBe(1)
})
