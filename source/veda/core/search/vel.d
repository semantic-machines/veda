/**
 * VQL parser
 */
module veda.core.search.vel;

// VEDA EXPRESSION LANG

private
{
    import std.string, std.array, std.stdio, std.conv, std.datetime, std.json, std.outbuffer, core.stdc.string;
    import veda.core.util.utils, veda.util.container;
}

//  expression
//  "==", "!="
//  "===" : поиск в подклассах
//  "=*" : полнотекстовый поиск
//  "&&", "||",
//  ">", "<", ">=", "<=",

protected bool delim(char c)
{
    return c == ' ' || c == '	'|| c == '\r' || c == '\n';
}

private string is_op(string c)
{
    if (c.length == 1)
    {
        if (c[ 0 ] == '>')
            return ">";

        if (c[ 0 ] == '<')
            return "<";
    }
    else if (c.length == 2)
    {
        if (c == ">=" || c == "<=" || c == "==" || c == "!=" || c == "=*" || c == "=+" || c == "||" || c == "&&")
            return c;

        if (c[ 0 ] == '>' && c[ 1 ] != '=')
            return ">";

        if (c[ 0 ] == '<' && c[ 1 ] != '=')
            return "<";
    }
    else if (c.length == 3)
    {
        if (c == "===")
            return c;
    }
    return null;
}

private int priority(string op)
{
    if (op == "<" || op == "<=" || op == ">" || op == "=>")
        return 4;

    if (op == "==" || op == "!=" || op == "=*" || op == "=+" || op == "===")
        return 3;

    if (op == "&&")
        return 2;

    if (op == "||")
        return 1;

    return -1;
}

private void process_op(ref stack!TTA st, string op)
{
    TTA r = st.popBack();
    TTA l = st.popBack();

    switch (op)
    {
    case "<":  st.pushBack(new TTA(op, l, r));  break;

    case ">":  st.pushBack(new TTA(op, l, r));  break;

    case "==":  st.pushBack(new TTA(op, l, r));  break;

    case "===":  st.pushBack(new TTA(op, l, r));  break;

    case "!=":  st.pushBack(new TTA(op, l, r));  break;

    case "=*":  st.pushBack(new TTA(op, l, r));  break;

    case "=+":  st.pushBack(new TTA(op, l, r));  break;

    case ">=":  st.pushBack(new TTA(op, l, r));  break;

    case "<=":  st.pushBack(new TTA(op, l, r));  break;

    case "||":  st.pushBack(new TTA(op, l, r));  break;

    case "&&":  st.pushBack(new TTA(op, l, r));  break;

    default:
    }
}

enum Decor
{
    NONE,
    QUOTED,
    RANGE
}

class TTA
{
    string op;

    Decor  token_decor = Decor.NONE;

    TTA    L;
    TTA    R;
    int    count = 0;

    this(string _op, TTA _L, TTA _R, Decor _token_decor = Decor.NONE)
    {
        op          = _op;
        L           = _L;
        R           = _R;
        token_decor = _token_decor;
    }

    override public string toString()
    {
        string res = "{";

        if (L !is null)
            res ~= L.toString();

        if (token_decor == Decor.QUOTED)
            res ~= "\"" ~ op ~ "\"";
        else if (token_decor == Decor.RANGE)
            res ~= " RANGE (" ~ op ~ ")";
        else
            res ~= op;

        if (R !is null)
            res ~= R.toString();

        return res ~ "}";
    }
}

public TTA parse_expr(string s)
{
    stack!TTA st    = new stack!TTA();
    stack!string op = new stack!string();

    for (int i = 0; i < s.length; i++)
    {
        if (!delim(s[ i ]))
        {
            if (s[ i ] == '(')
                op.pushBack("(");
            else if (s[ i ] == ')')
            {
                while (!op.empty() && op.back() != "(")
                    process_op(st, op.popBack());
                op.popBack();
            }
            else
            {
                int e = i + 2;
                if (e > s.length)
                    e = cast(int)(s.length - 1);

                if (s[ i ] == '=' && s[ e ] == '=')
                    e++;

                string curop = is_op(s[ i .. e ]);
                if (curop !is null)
                {
                    while (!op.empty() && priority(op.back()) >= priority(curop))
                        process_op(st, op.popBack());
                    op.pushBack(curop);
                    i += curop.length - 1;
                }
                else
                {
                    string operand;

                    while (i < s.length && s[ i ] == ' ')
                        i++;

                    if (s[ i ] == '\'')
                    {
                        i++;
                        int bp = i;
                        while (i < s.length && s[ i ] != '\'')
                            i++;
                        operand = s[ bp .. i ];
                        st.pushBack(new TTA(operand, null, null, Decor.QUOTED));
                    }
                    else if (s[ i ] == '`')
                    {
                        i++;
                        int bp = i;
                        while (i < s.length && s[ i ] != '`')
                            i++;
                        operand = s[ bp .. i ];
                        st.pushBack(new TTA(operand, null, null, Decor.QUOTED));
                    }
                    else if (s[ i ] == '[')
                    {
                        i++;
                        int bp = i;
                        while (i < s.length && s[ i ] != ']')
                            i++;
                        operand = s[ bp .. i ];
                        st.pushBack(new TTA(operand, null, null, Decor.RANGE));
                    }
                    else
                    {
                        // no quote
                        int bp = i;
                        while (i < s.length && s[ i ] != ' ' && s[ i ] != '&' && s[ i ] != '|' && s[ i ] != '=' && s[ i ] != '<' &&
                               s[ i ] != '>' && s[ i ] != '!' && s[ i ] != '-' && s[ i ] != ' ')
                            i++;

                        int ep = i;

                        if (s[ ep - 1 ] == '(' || s[ ep - 1 ] == ')')
                        {
                            ep = i - 1;
                            i -= 2;
                        }

                        operand = s[ bp .. i ];
                        st.pushBack(new TTA(operand, null, null));
                    }
                }
            }
        }
    }
    while (!op.empty())
        process_op(st, op.popBack());

    return st.back();
}
