const Joi = require('@hapi/joi')
const WhereSchema = require('./WhereSchema')
const OrderBySchema = require('./OrderBySchema')

const ColumnsSchema = Joi.object({
    name: Joi.string().pattern(/^\w+$/).max(64).required().messages({
        'string.pattern.base':
            'One of the column names is invalid. Only a-z,A-Z,0-9 and _ allowed.',
        'string.max': 'One of the column names is too long',
        'any.required': 'A column name is missing',
    }),
})

const SelectSchema = Joi.object({
    name: Joi.string().required().valid('SELECT').insensitive().messages({
        'any.only': 'Query must begin with SELECT',
        'any.required': 'Query must begin with SELECT',
    }),

    size: Joi.number().positive().required().messages({}),

    parserCounter: Joi.number()
        .required()
        .min(Joi.ref('size'))
        .messages({
            'number.min':
                'The query is longer than expected. There is something additional or wrongly formatted in the query',
        })
        .optional(),

    columns: Joi.array().min(1).items(ColumnsSchema).required().messages({
        'array.base': 'this is not an array',
        'array.min': 'There should be at least one column specified',
        'any.required': 'There should be at least one column specified',
    }),

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

const SelectColumnsOrderBySchema = SelectSchema.keys({
    orderBy: OrderBySchema,
})

const SelectColumnsWhereSchema = SelectSchema.keys({
    where: WhereSchema,
})

const SelectColumnsWhereOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    orderBy: OrderBySchema,
})

module.exports = {
    SelectSchema,
    SelectColumnsOrderBySchema,
    SelectColumnsWhereSchema,
    SelectColumnsWhereOrderBySchema,
}
