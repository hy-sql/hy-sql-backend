const Joi = require('@hapi/joi')
const WhereAdvancedSchema = require('./WhereAdvancedSchema')
const OrderBySchema = require('./OrderBySchema')
// const OrderBySchema = require('./OrderBySchema')

const {
    stringFunctionsNamePattern,
    stringFunctionPattern,
    aggregateFunctionsNamePattern,
    aggregateFunctionPattern,
} = require('../utils/regex')

const SelectAdvancedSchema = Joi.object({
    name: Joi.string().valid('SELECT ADVANCED'),

    fields: Joi.array()
        .items(
            Joi.object({
                type: Joi.string().valid('all').required(),
                value: Joi.string().valid('*').required(),
            }),
            Joi.object({
                type: Joi.string().valid('column').required(),
                value: Joi.string().pattern(/^\w+$/).required(),
            }),
            Joi.object({
                type: Joi.string().valid('expression').required(),
                value: Joi.array(),
                stringValue: Joi.string(),
            }),
            Joi.object({
                type: Joi.string().valid('stringFunction').required(),
                name: Joi.string()
                    .pattern(stringFunctionsNamePattern)
                    .required(),
                value: Joi.string()
                    .pattern(stringFunctionPattern)
                    .insensitive()
                    .required(),
                param: Joi.object().required(),
            }),
            Joi.object({
                type: Joi.string().valid('aggregateFunction').required(),
                name: Joi.string()
                    .pattern(aggregateFunctionsNamePattern)
                    .required(),
                value: Joi.string()
                    .pattern(aggregateFunctionPattern)
                    .insensitive()
                    .required(),
                param: Joi.string()
                    .pattern(/^('?\w+'?|\*)$/)
                    .required(),
            })
        )
        .required()
        .messages({}),

    from: Joi.string().required().valid('FROM').insensitive().messages({
        'any.only':
            'In a SELECT-query the column names must be followed by FROM',
        'any.required':
            'In a SELECT-query the column names must be followed by FROM',
    }),

    tableName: Joi.string().required().pattern(/^\w+$/).max(64).messages({
        'any.required': 'Query must contain a table name',
        'string.pattern.base':
            'Table name should only contain one or more alphanumeric characters and underscores',
        'string.max': 'The table name is too long',
    }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

const SelectAdvancedWhereSchema = SelectAdvancedSchema.keys({
    where: WhereAdvancedSchema,
})

const SelectAdvancedOrderBySchema = SelectAdvancedSchema.keys({
    orderBy: OrderBySchema,
})

const SelectAdvancedWhereOrderBySchema = SelectAdvancedSchema.keys({
    where: WhereAdvancedSchema,
    orderBy: OrderBySchema,
})

module.exports = {
    SelectAdvancedSchema,
    SelectAdvancedWhereSchema,
    SelectAdvancedOrderBySchema,
    SelectAdvancedWhereOrderBySchema,
}
