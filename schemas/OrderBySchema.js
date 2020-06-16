const Joi = require('@hapi/joi')
const { ColumnSchema } = require('./FieldSchemas')
const FunctionSchema = require('./FunctionSchema')
const ExpressionSchema = require('./ExpressionSchema')

const OrderByColumnSchema = ColumnSchema.keys({
    order: Joi.object({
        type: Joi.string().valid('order').required(),
        value: Joi.string().valid('asc', 'desc'),
    }).optional(),
})

const OrderByFunctionSchema = FunctionSchema.keys({
    order: Joi.object({
        type: Joi.string().valid('order').required(),
        value: Joi.string().valid('asc', 'desc'),
    }).optional(),
})

const OrderByExpressionSchema = ExpressionSchema.keys({
    order: Joi.object({
        type: Joi.string().valid('order').required(),
        value: Joi.string().valid('asc', 'desc'),
    }).optional(),
})

const OrderBySchema = Joi.object({
    keyword: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ORDER BY$/)
        .insensitive()
        .optional()
        .messages({
            'any.required':
                'This query is expected to contain the following keyword: ORDER BY',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'ORDER BY is either misspelled, missing or in the wrong position',
        }),

    fields: Joi.array()
        .items(
            OrderByColumnSchema,
            OrderByFunctionSchema,
            OrderByExpressionSchema
        )
        .min(1),
})

module.exports = OrderBySchema
