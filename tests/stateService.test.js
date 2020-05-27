const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')

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
    const rows = state.tables[0].rows
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

test('SELECT * FROM ORDER BY returns rows from table in ascending order', () => {
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

    console.log(parsedSelectAllOrderByCommand)

    const result = stateService.selectAllFromTable(
        parsedSelectAllOrderByCommand.value
    )

    expect(result.rows).toEqual(expectedRows)
})
