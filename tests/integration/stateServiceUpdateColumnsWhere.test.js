const State = require('../../models/State')
const StateService = require('../../services/StateService')
const commandService = require('../../services/commandService')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each([
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris';",
    "update Tuotteet SET hinta=6 where nimi='nauris';",
    "update Tuotteet set hinta=6 WHeRe nimi='nauris';",
])('Valid UPDATE command testing', (command) => {
    describe(command, () => {
        const tables = new Map([
            [
                'Tuotteet',
                {
                    name: 'Tuotteet',
                    columns: [
                        {
                            name: 'id',
                            type: 'INTEGER',
                            constraints: ['PRIMARY KEY'],
                        },
                        {
                            name: 'nimi',
                            type: 'TEXT',
                            constraints: [],
                        },
                        {
                            name: 'hinta',
                            type: 'INTEGER',
                            constraints: [],
                        },
                    ],
                    rows: [
                        {
                            id: 1,
                            nimi: 'nauris',
                            hinta: 7,
                        },
                        {
                            id: 2,
                            nimi: 'omena',
                            hinta: 5,
                        },
                    ],
                },
            ],
        ])
        const state = new State(tables)
        const stateService = new StateService(state)

        const fullCommandAsStringArray = splitCommandIntoArray(command)
        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringArray
        )

        test('returns correct result message from stateService', () => {
            const result = stateService.updateState(parsedCommand.value)

            expect(result.result).toBe('Rows in table Tuotteet updated')
            expect(result.error).not.toBeDefined()
        })

        test('updates the correct values from the row', () => {
            const rows = state.getTableByName('Tuotteet').rows

            expect(rows[0].hinta).toBe(6)
            expect(rows[0].nimi).toBe('nauris')
            expect(rows[1].hinta).toBe(5)
            expect(rows[1].nimi).toBe('omena')
        })
    })
})

describe.each([
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris';",
    "update Tuotteet SET hinta=6 where nimi='nauris';",
    "update Tuotteet set hinta=6 WHeRe nimi='nauris';",
])('If table does not exist ', (command) => {
    describe(command, () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const fullCommandAsStringArray = splitCommandIntoArray(command)
        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringArray
        )

        test('returns error message from stateService', () => {
            const result = stateService.updateState(parsedCommand.value)

            expect(result.result).not.toBeDefined()
            expect(result.error).toBe('No such table Tuotteet')
        })
    })
})

describe.each([
    "UPDATE Tuotteet SET nimi=6 where nimi='nauris';",
    "UPDATE Tuotteet SET nimi=6 WHERE nimi='nauris' AND hinta=7;",
])('Trying to update wrong type of data', (command) => {
    describe(command, () => {
        const tables = new Map([
            [
                'Tuotteet',
                {
                    name: 'Tuotteet',
                    columns: [
                        {
                            name: 'id',
                            type: 'INTEGER',
                            constraints: ['PRIMARY KEY'],
                        },
                        {
                            name: 'nimi',
                            type: 'TEXT',
                            constraints: [],
                        },
                        {
                            name: 'hinta',
                            type: 'INTEGER',
                            constraints: [],
                        },
                    ],
                    rows: [
                        {
                            id: 1,
                            nimi: 'retiisi',
                            hinta: 7,
                        },
                        {
                            id: 2,
                            nimi: 'omena',
                            hinta: 5,
                        },
                    ],
                },
            ],
        ])
        const state = new State(tables)
        const stateService = new StateService(state)

        const fullCommandAsStringArray = splitCommandIntoArray(command)
        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringArray
        )

        test('returns error message from stateService', () => {
            const result = stateService.updateState(parsedCommand.value)

            expect(result.result).not.toBeDefined()
            expect(result.error).toBe(
                'Wrong datatype: expected TEXT but was INTEGER'
            )
        })

        test('does not update values in the row', () => {
            const rows = state.getTableByName('Tuotteet').rows

            expect(rows[0].hinta).toBe(7)
            expect(rows[0].nimi).toBe('retiisi')
            expect(rows[1].hinta).toBe(5)
            expect(rows[1].nimi).toBe('omena')
        })
    })
})

describe.each([
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris' AND hinta=7;",
    "update Tuotteet SET hinta=6 where nimi='nauris' AND hinta=7;",
    "update Tuotteet set hinta=6 WHeRe nimi='nauris' AND hinta=7;",
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='TUOTE' OR hinta=7;",
    "UPDATE Tuotteet SET hinta=6 WHERE (nimi='TUOTE' AND hinta=5) OR hinta=7;",
    "UPDATE Tuotteet SET hinta=6 WHERE (nimi='nauris' AND hinta=7) OR hinta=10;",
])('Valid UPDATE command testing', (command) => {
    describe(command, () => {
        const tables = new Map([
            [
                'Tuotteet',
                {
                    name: 'Tuotteet',
                    columns: [
                        {
                            name: 'id',
                            type: 'INTEGER',
                            constraints: ['PRIMARY KEY'],
                        },
                        {
                            name: 'nimi',
                            type: 'TEXT',
                            constraints: [],
                        },
                        {
                            name: 'hinta',
                            type: 'INTEGER',
                            constraints: [],
                        },
                    ],
                    rows: [
                        {
                            id: 1,
                            nimi: 'nauris',
                            hinta: 7,
                        },
                        {
                            id: 2,
                            nimi: 'omena',
                            hinta: 5,
                        },
                    ],
                },
            ],
        ])
        const state = new State(tables)
        const stateService = new StateService(state)

        const fullCommandAsStringArray = splitCommandIntoArray(command)
        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringArray
        )

        test('returns correct result message from stateService', () => {
            const result = stateService.updateState(parsedCommand.value)

            expect(result.result).toBe('Rows in table Tuotteet updated')
            expect(result.error).not.toBeDefined()
        })

        test('updates the correct values from the row', () => {
            const rows = state.getTableByName('Tuotteet').rows

            expect(rows[0].hinta).toBe(6)
            expect(rows[0].nimi).toBe('nauris')
            expect(rows[1].hinta).toBe(5)
            expect(rows[1].nimi).toBe('omena')
        })
    })
})
