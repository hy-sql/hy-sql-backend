const Joi = require('@hapi/joi')
const { ColumnSchema, IntegerSchema, TextSchema } = require('./FieldSchemas')
const { ExpressionSchema } = require('./ExpressionSchema')
const FunctionSchema = require('./FunctionSchema')
const { comparisonOperatorPattern } = require('../helpers/regex')

/**
 * Joi schema for validating conditions.
 */
const ConditionSchema = Joi.object({
    left: Joi.alternatives()
        .try(
            ColumnSchema,
            IntegerSchema,
            TextSchema,
            ExpressionSchema.shared(FunctionSchema),
            FunctionSchema.shared(ExpressionSchema)
        )
        .required(),
    operator: Joi.string().pattern(comparisonOperatorPattern).required(),
    right: Joi.alternatives()
        .try(
            ColumnSchema,
            IntegerSchema,
            TextSchema,
            ExpressionSchema.shared(FunctionSchema),
            FunctionSchema.shared(ExpressionSchema)
        )
        .required(),
}).id('conditionSchema')

/**
 * Joi schema for validating conditions in WHERE objects.
 */
const ConditionsSchema = Joi.object({
    AND: Joi.link('#conditionsArray'),
    OR: Joi.link('#conditionsArray'),
})
    .required()
    .id('conditionsSchema')

const conditionsArraySchema = Joi.array()
    .items(Joi.link('#conditionSchema'), Joi.link('#conditionsSchema'))
    .id('conditionsArray')
    .messages({
        'array.includes': 'Not a condition',
    })

/**
 * Joi schema for validating WHERE objects.
 */
const WhereSchema = Joi.object({
    keyword: Joi.string()
        .pattern(/;/, { invert: true })
        .pattern(/^WHERE$/i)
        .required(),

    conditions: ConditionsSchema.shared(
        conditionsArraySchema.shared(ConditionSchema).shared(ConditionsSchema)
    ),
})

module.exports = WhereSchema
