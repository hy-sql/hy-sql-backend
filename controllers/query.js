const queryRouter = require('express').Router()

// POST	/api/query	Vastaanottaa merkkijonoja sisältävän JSON objektin { query: ['...', ...] }
//                  ja palauttaa merkkijonoja sisältävän JSON objektin { result: ['...', ...] }

queryRouter.post('/', (req, res) => {
    const body = req.body

    if (!body.query) {
        return res.status(400).json({
            error: 'query missing'
        })
    }

    const query = {
        query: body.query
    }

    res.json(query)
})

module.exports = queryRouter