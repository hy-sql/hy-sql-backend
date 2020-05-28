const Joi = require('@hapi/joi')
const OrderBySchema = require('./OrderBySchema')
const WhereSchema = require('./WhereSchema')

const SelectAllSchema = Joi.object({
    name: Joi.string().required().valid('SELECT *').insensitive().messages({
        'any.only': 'Query must begin with SELECT *',
        'any.required': 'Query must begin with SELECT *',
    }),

    from: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^[F,f][R,r][O,o][M,m]$/)
        .messages({
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base': 'SELECT * must be followed by FROM',
            'any.required': 'SELECT * must be followed by FROM',
        }),

    tableName: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .messages({
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'any.required': 'Query must contain a table name',
            'string.pattern.base':
                'Table name should only contain one or more alphanumeric characters and underscores',
            'string.max': 'Table name is too long',
        }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),
})

const SelectAllOrderBySchema = SelectAllSchema.keys({
    orderBy: OrderBySchema,
})

const SelectAllWhereSchema = SelectAllSchema.keys({
    where: WhereSchema,
})

module.exports = {
    SelectAllSchema,
    SelectAllOrderBySchema,
    SelectAllWhereSchema,
}
