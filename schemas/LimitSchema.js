const Joi = require('@hapi/joi')
const { IntegerSchema } = require('./FieldSchemas')
const { SimplerExpressionSchema } = require('./ExpressionSchema')

/**
 * Joi schema for validating OFFSET objects in LIMIT objects.
 */
const OffsetSchema = Joi.object({
    keyword: Joi.string()
        .pattern(/^OFFSET$/i)
        .required(),

    field: Joi.alternatives()
        .try(IntegerSchema, SimplerExpressionSchema)
        .required()
        .messages({
            'any.required':
                'OFFSET must be followed by the number of rows to be returned',
            'alternatives.match':
                'Value following OFFSET should be either a number or an arithmetic expression',
        }),
})

/**
 * Joi schema for validating LIMIT objects.
 */
const LimitSchema = Joi.object({
    keyword: Joi.string()
        .pattern(/^LIMIT$/i)
        .required(),

    field: Joi.alternatives()
        .try(IntegerSchema, SimplerExpressionSchema)
        .required()
        .messages({
            'any.required':
                'LIMIT must be followed by the number of rows to be returned',
            'alternatives.match':
                'Value following LIMIT should be either a number or an arithmetic expression',
        }),

    offset: OffsetSchema,
})

module.exports = { LimitSchema }
