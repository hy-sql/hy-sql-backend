const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')

describe.each([
    "UPDATE Tuotteet SET hinta=6 WHERE nimi='nauris';",
    "update Tuotteet SET hinta=6 where nimi='nauris';",
    "update Tuotteet set hinta=6 WHeRe nimi='nauris';",
])('Valid UPDATE command testing', (command) => {
    describe(command, () => {
        const initArray = [
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
        ]
        const state = new State(initArray)
        const stateService = new StateService(state)

        const fullCommandAsStringList = command
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        const parsedCommand = commandService.parseCommand(
            fullCommandAsStringList
        )

        test('returns correct result message from stateService', () => {
            const result = stateService.updateState(parsedCommand.value)

            expect(result.result).toBe('Rows in table Tuotteet updated')
            expect(result.error).not.toBeDefined()
        })

        test('updates the correct values from the row', () => {
            const rows = state.tables[0].rows

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
        const initArray = []
        const state = new State(initArray)
        const stateService = new StateService(state)

        const fullCommandAsStringList = command
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

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

describe.each(["UPDATE Tuotteet SET nimi=6 where nimi='nauris';"])(
    'Trying to update wrong type of data',
    (command) => {
        describe(command, () => {
            const initArray = [
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
            ]
            const state = new State(initArray)
            const stateService = new StateService(state)

            const fullCommandAsStringList = command
                .trim()
                .replace(/\s\s+/g, ' ')
                .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

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
