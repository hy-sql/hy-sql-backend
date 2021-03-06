const Joi = require('@hapi/joi')
const { ColumnSchema } = require('./FieldSchemas')
const FunctionSchema = require('./FunctionSchema')
const { ExpressionSchema } = require('./ExpressionSchema')

/**
 * Joi schema for validating fields in ORDER BY objects.
 */
const OrderByColumnSchema = ColumnSchema.keys({
    order: Joi.object({
        type: Joi.string().valid('order').required(),
        value: Joi.string().valid('asc', 'desc'),
    }).optional(),
})

/**
 * Joi schema for validating fields containing functions in ORDER BY objects.
 */
const OrderByFunctionSchema = FunctionSchema.keys({
    order: Joi.object({
        type: Joi.string().valid('order').required(),
        value: Joi.string().valid('asc', 'desc'),
    }).optional(),
})

/**
 * Joi schema for validating fields containing expressions in ORDER BY objects.
 */
const OrderByExpressionSchema = ExpressionSchema.keys({
    order: Joi.object({
        type: Joi.string().valid('order').required(),
        value: Joi.string().valid('asc', 'desc'),
    }).optional(),
})

/**
 * Joi schema for validating ORDER BY objects.
 */
const OrderBySchema = Joi.object({
    keyword: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ORDER BY$/i)
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
