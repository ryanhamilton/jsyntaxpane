/*
 * Copyright 2020 Ryan Hamilton @ TimeStored
 */
package jsyntaxpane.syntaxkits;

import jsyntaxpane.DefaultSyntaxKit;
import jsyntaxpane.lexers.SqlLexer;

/**
 * @author Ryan Hamilton
 */
public class QSqlSyntaxKit extends DefaultSyntaxKit {

    public QSqlSyntaxKit() {
        super(new QSqlLexer());
    }
}
