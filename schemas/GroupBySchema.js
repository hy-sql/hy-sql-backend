const Joi = require('@hapi/joi')
const { ColumnSchema } = require('./FieldSchemas')
const FunctionSchema = require('./FunctionSchema')
const ExpressionSchema = require('./ExpressionSchema')

const GroupBySchema = Joi.object({
    keyword: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^GROUP BY$/i)
        .messages({
            'any.required':
                'This query is expected to contain the following keyword: ORDER BY',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'ORDER BY is either misspelled, missing or in the wrong position',
        }),

    fields: Joi.array()
        .items(ColumnSchema, FunctionSchema, ExpressionSchema)
        .min(1),
})

module.exports = GroupBySchema
