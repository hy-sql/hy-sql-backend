const Joi = require('@hapi/joi')
const { IntegerSchema } = require('./FieldSchemas')
const ExpressionSchema = require('./ExpressionSchema')

/**
 * Joi schema for validating OFFSET objects in LIMIT objects.
 */
const OffsetSchema = Joi.object({
    keyword: Joi.string()
        .pattern(/^OFFSET$/i)
        .required(),

    field: Joi.alternatives()
        .try(IntegerSchema, ExpressionSchema)
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
    correctlyPlaced: Joi.boolean().valid(true).required().messages({
        'any.only':
            'OFFSET must always be after LIMIT and LIMIT can not be before WHERE, GROUP BY or ORDER BY',
    }),

    keyword: Joi.string()
        .pattern(/^LIMIT$/i)
        .required(),

    field: Joi.alternatives()
        .try(IntegerSchema, ExpressionSchema)
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
