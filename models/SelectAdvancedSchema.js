const Joi = require('@hapi/joi')
// const WhereSchema = require('./WhereSchema')
// const OrderBySchema = require('./OrderBySchema')

const arithmeticPattern = /^\w+(( )?[+-/*%]( )?\w+)+$/
const functionPattern = /^LENGTH\(\w+\)$/

const SelectAdvancedSchema = Joi.object({
    name: Joi.string().valid('SELECT ADVANCED'),

    fields: Joi.array()
        .items(
            Joi.object({
                type: Joi.string().valid('column').required(),
                value: Joi.string().pattern(/^\w+$/).required(),
            }),
            Joi.object({
                type: Joi.string().valid('expression').required(),
                value: Joi.string().pattern(arithmeticPattern).required(),
            }),
            Joi.object({
                name: Joi.string().valid('LENGTH').required(),
                type: Joi.string().valid('function').required(),
                value: Joi.string().pattern(functionPattern).required(),
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

const SelectWithOperatorsOrderBySchema = SelectAdvancedSchema.keys({})

const SelectWithOperatorsWhereSchema = SelectAdvancedSchema.keys({})

const SelectWithOperatorsWhereOrderBySchema = SelectAdvancedSchema.keys({})

module.exports = {
    SelectAdvancedSchema,
    SelectWithOperatorsOrderBySchema,
    SelectWithOperatorsWhereSchema,
    SelectWithOperatorsWhereOrderBySchema,
}
