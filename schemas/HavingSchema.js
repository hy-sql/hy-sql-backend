const Joi = require('@hapi/joi')
const { ColumnSchema } = require('./FieldSchemas')
const FunctionSchema = require('./FunctionSchema')
const { ExpressionSchema } = require('./ExpressionSchema')

const HavingSchema = Joi.object({
    keyword: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^HAVING$/i)
        .messages({
            'any.required':
                'This query is expected to contain the following keyword: HAVING',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'HAVING is either misspelled, missing or in the wrong position',
        }),

    fields: Joi.array()
        .items(ColumnSchema, FunctionSchema, ExpressionSchema)
        .min(1),
})

module.exports = HavingSchema
