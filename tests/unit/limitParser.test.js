const { parseLimit } = require('../../commandParsers/limitParser')
const {
    queryContainsLimitKeyword,
} = require('../../commandParsers/parserTools/queryContains')
const { LimitSchema } = require('../../schemas/LimitSchema')
const splitCommandIntoArray = require('../../commandParsers/parserTools/splitCommandIntoArray')

describe.each(['LIMIT 2', 'LIMIT 20', 'LIMIT 2 + 2', 'LIMIT 2*5+3'])(
    'Valid LIMIT part of a valid query',
    (validCommand) => {
        describe(validCommand, () => {
            const command = splitCommandIntoArray(validCommand)

            test('is recognised to contain a LIMIT keyword', () => {
                expect(queryContainsLimitKeyword(command)).toBeTruthy()
            })

            test('is parsed and validated succesfully', () => {
                const parsedCommand = LimitSchema.validate(parseLimit(command))

                expect(parsedCommand.value).toHaveProperty('keyword')
                expect(parsedCommand.value).toHaveProperty('field')
                expect(parsedCommand.value).not.toHaveProperty('offset')
                expect(parsedCommand.error).toBeUndefined()
            })
        })
    }
)

describe.each([
    'LIMIT 2 OFFSET 2',
    'LIMIT 2 OFFSET 30',
    'LIMIT 2 OFFSET 2 + 2',
    'LIMIT 2 OFFSET 2*5+3',
])('Valid LIMIT OFFSET part of a valid query', (validCommand) => {
    describe(validCommand, () => {
        const command = splitCommandIntoArray(validCommand)

        test('is parsed and validated succesfully', () => {
            const parsedCommand = LimitSchema.validate(parseLimit(command))

            expect(parsedCommand.value).toHaveProperty('keyword')
            expect(parsedCommand.value).toHaveProperty('field')
            expect(parsedCommand.value).toHaveProperty('offset')
            expect(parsedCommand.value.offset).toHaveProperty('keyword')
            expect(parsedCommand.value.offset).toHaveProperty('field')
            expect(parsedCommand.error).toBeUndefined()
        })
    })
})

describe.each([
    'LIMIT column',
    "LIMIT '24'",
    'LIMIT 2a+2',
    'LIMIT 2 OFFSET column',
    "LIMIT 2 OFFSET '24'",
    'LIMIT 2 OFFSET 2a*2',
])('Invalid LIMIT or LIMIT OFFSET part of a query', (invalidCommand) => {
    describe(invalidCommand, () => {
        const command = splitCommandIntoArray(invalidCommand)

        test('is recognised to contain LIMIT keyword', () => {
            expect(queryContainsLimitKeyword(command)).toBeTruthy()
        })

        test('fails validation after parsing', () => {
            const parsedCommand = LimitSchema.validate(parseLimit(command))

            expect(parsedCommand.value).toBeDefined()
            expect(parsedCommand.error).toBeDefined()
        })
    })
})
