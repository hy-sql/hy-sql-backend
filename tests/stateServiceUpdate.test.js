const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')
const cleanCommand = require('../utils/cleanCommand')

describe.each([
    "UPDATE Tuotteet SET hinta=6, nimi='nauris';",
    "update Tuotteet SET hinta=6, nimi='nauris';",
    "update Tuotteet set hinta=6, nimi='nauris';",
    "uPdAtE Tuotteet sEt hinta=6, nimi='nauris';",
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

        const fullCommandAsStringList = cleanCommand(command)
        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringList
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
            expect(rows[1].hinta).toBe(6)
            expect(rows[1].nimi).toBe('nauris')
        })
    })
})

describe.each([
    "UPDATE Tuotteet SET hinta=6, nimi='nauris';",
    "update Tuotteet SET hinta=6, nimi='nauris';",
    "update Tuotteet set hinta=6, nimi='nauris';",
    "uPdAtE Tuotteet sEt hinta=6, nimi='nauris';",
])('If table does not exist ', (command) => {
    describe(command, () => {
        const state = new State(new Map())
        const stateService = new StateService(state)

        const fullCommandAsStringList = cleanCommand(command)

        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringList
        )

        test('returns error message from stateService', () => {
            const result = stateService.updateState(parsedCommand.value)

            expect(result.result).not.toBeDefined()
            expect(result.error).toBe('No such table Tuotteet')
        })
    })
})

describe.each(['UPDATE Tuotteet SET nimi=6;'])(
    'Trying to update wrong type of data',
    (command) => {
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

            const fullCommandAsStringList = cleanCommand(command)
            const parsedCommand = commandService.parseCommand(
                fullCommandAsStringList
            )

            test('returns error message from stateService', () => {
                const result = stateService.updateState(parsedCommand.value)

                expect(result.result).not.toBeDefined()
                expect(result.error).toBe(
                    'Wrong datatype: expected TEXT but was INTEGER'
                )
            })
        })
    }
)
