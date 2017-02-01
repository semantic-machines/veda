/*
    author: https://github.com/SirTony
    code: https://raw.githubusercontent.com/SirTony/commando/0b2af857dc69bcf175b7a374dd0a99d196ccb4e0/src/commando.d
*/

module commando;

import std.uni;
import std.conv;
import std.path;
import std.range;
import std.regex;
import std.stdio;
import std.string;
import std.traits;
import std.variant;
import std.typecons;
import std.algorithm;
import std.exception;

import core.stdc.stdlib;

final class ArgumentParserException : Exception
{
    mixin basicExceptionCtors;
}

private enum hasSignature( alias fun, TRet, TArgs... )
    =
        isCallable!fun
    &&  is( ReturnType!( fun ) == TRet )
    &&  is( typeof( { fun( TArgs.init ); } ) );

unittest
{
    int add( int a, int b ) { return a + b; }
    static real pow( real n, int exp ) { return n ^^ exp; }

    assert( hasSignature!( add, int, int, int ) );
    assert( hasSignature!( pow, real,  real, int ) );
}

private final class Option
{
    private
    {
        alias TParser = Variant delegate( string );
        alias TAssigner = void delegate( Variant );

        TParser parser;
        TAssigner assigner;
        bool _handled;
        TypeInfo _type;
    }

    bool handled() @property
    {
        return _handled;
    }

    TypeInfo type() @property
    {
        return _type;
    }

    Required required;
    char shortName;
    string longName;
    string helpText;

    this( TPtr, TParse )( Required required, char shortName, string longName, string helpText, TParse parser, TPtr* pointer )
        if( hasSignature!( parser, TPtr, string ) )
    {
        Variant parserWrapper( string value )
        {
            return Variant( parser( value ) );
        }

        void assigner( Variant value )
        {
            *pointer = value.get!TPtr;
        }

        _type = typeid( TPtr );

        this.parser = &parserWrapper;
        this.assigner = &assigner;

        this.required = required;
        this.shortName = shortName;
        this.longName = longName;
        this.helpText = helpText;
    }

    bool hasShortName() @property
    {
        return this.shortName != '\0';
    }

    Option opAssign( string value )
    {
        if( _handled )
            return this;

        auto parsed = this.parser( value );
        this.assigner( parsed );
        _handled = true;

        return this;
    }

    Option opAssign( bool value )
    {
        if( _handled )
            return this;

        this.assigner( Variant( value ) );
        _handled = true;

        return this;
    }
}

private alias CommandCallback = void delegate();
private final class Command
{
    string command;
    string helpText;
    ArgumentSyntax syntax;
    private CommandCallback callback;

    this( TFun )( string command, string helpText, ArgumentSyntax syntax, TFun callback )
    {
        void callbackWrapper()
        {
            static if( !is( TFun == typeof( null ) ) )
                callback();
        }

        this.command = command;
        this.helpText = helpText;
        this.syntax = syntax;
        this.callback = &callbackWrapper;
    }

    void invoke()
    {
        this.callback();
    }
}

alias Required           = Flag!"Required";
alias AllowBundling      = Flag!"AllowBundling";
alias IgnoreUnrecognized = Flag!"IgnoreUnrecognized";
alias CaseSensitive      = Flag!"CaseSensitive";

private struct ArgumentParserConfig
{
    AllowBundling allowBundling;
    IgnoreUnrecognized ignoreUnrecognizedOptions;
    CaseSensitive caseSensitive;
}

final class ArgumentSyntax
{
    private Command[string] commands;
    private Option[] options;

    ArgumentParserConfig config;

    private this()
    {
        config = ArgumentParserConfig(
            AllowBundling.yes,
            IgnoreUnrecognized.yes,
            CaseSensitive.no
        );
    }

    private bool tryFind( string longName, out Option option )
    {
        bool caseSensitive = config.caseSensitive == CaseSensitive.yes;
        if( !caseSensitive )
            longName = longName.toLower;

        auto result = this.options.filter!( ( Option option ) {
            auto name = !caseSensitive ? option.longName.toLower : option.longName;
            return name == longName;
        } ).array;

        if( result.length == 0 )
            return false;

        option = result.front;
        return true;
    }

    private bool tryFind( char shortName, out Option option )
    {
        bool caseSensitive = config.caseSensitive == CaseSensitive.yes;
        if( !caseSensitive )
            shortName = cast(char)shortName.toLower;

        auto result = this.options.filter!( o => o.hasShortName )
                                  .filter!( ( Option option ) {
                                      char name = !caseSensitive ? cast(char)option.shortName.toLower : option.shortName;
                                      return name == shortName;
                                  } ).array;

        if( result.length == 0 )
            return false;

        option = result.front;
        return true;
    }

    void option( TVal )( char shortName, string longName, TVal* value, Required required, string helpText )
    {
        auto _default = &( defaultParser!TVal );
        this.option( shortName, longName, value, _default, required, helpText );
    }

    void option( TVal )( string longName, TVal* value, Required required, string helpText )
    {
        auto _default = &( defaultParser!TVal );
        this.option( '\0', longName, value, _default, required, helpText );
    }

    void option( TVal, TFun )( string longName, TVal* value, TFun parser, Required required, string helpText )
    {
        this.option( '\0', longName, value, parser, required, helpText );
    }

    void option( TVal, TFun )( char shortName, string longName, TVal* value, TFun parser, Required required, string helpText )
        if( hasSignature!( parser, TVal, string ) )
    {
        if( ( shortName.isControl && shortName != '\0' ) || shortName == ' ' )
            throw new ArgumentParserException( "Short name must be a printable character" );

        if( shortName == '?' || shortName == 'h' || shortName == 'H' )
            throw new ArgumentParserException( "'%s' is a reserved flag".format( shortName ) );

        if( longName is null || longName.length == 0 || longName.all!( c => c == ' ' || c.isControl ) )
            throw new ArgumentParserException( "Long name must consist of only printable characters and cannot be null" );

        if( longName.strip.toLower == "help" )
            throw new ArgumentParserException( "'%s' is a reserved flag".format( longName ) );

        Option option;
        if( this.tryFind( longName, option ) )
            throw new ArgumentParserException( "An option with the name '%s' has already been defined".format( longName ) );

        if( shortName != '\0' && this.tryFind( shortName, option ) )
            throw new ArgumentParserException( "An option with the name '%s' has already been defined".format( shortName ) );

        this.options ~= new Option( required, shortName, longName.strip, helpText, parser, value );
    }

    void command( TFun )( string command, string helpText, TFun builder )
        if( hasSignature!( builder, void, ArgumentSyntax ) )
    {
        this.command( command, null, helpText, builder );
    }

    void command( TFun, TCallback )( string command, TCallback callback, string helpText, TFun builder )
        if(
                ( hasSignature!( builder, void, ArgumentSyntax ) || is( TFun == typeof( null ) ) )
             && ( hasSignature!( callback, void ) || is( TCallback == typeof( null ) ) )
        )
    {
        if( command is null || command.length == 0 || command.all!( c => c == ' ' || c.isControl ) )
            throw new ArgumentParserException( "Command must consist of only printable characters and cannot be null" );

        auto syntax = new ArgumentSyntax;
        if( builder !is null )
            builder( syntax );

        this.commands[command] = new Command( command, helpText, syntax, callback );
    }
}

final class ArgumentParser
{
    private string appName;

    private this( string appName )
    {
        this.appName = appName;
    }

    static void parse( TFun )( string[] args, TFun builder )
        if( hasSignature!( builder, void, ArgumentSyntax ) )
    {
        auto syntax = new ArgumentSyntax();
        builder( syntax );

        auto r = args.map!( s => s.strip );

        auto appName = r.front.baseName.stripExtension; r.popFront;
        auto self = new ArgumentParser( appName );
        self.parseImpl( r, syntax, [] );
    }

    private void parseImpl( R )( R r, ArgumentSyntax syntax, string[] commandPath )
        if( isForwardRange!R && is( ElementType!( R ) == string ) )
    {
        if( r.empty )
            return;

        auto command = r.front in syntax.commands;

        if( command )
        {
            r.popFront;
            (*command).syntax.config = syntax.config;
            syntax = (*command).syntax;
        }

        bool helpRequested = r.empty ? false : [ "-h", "-?", "--help" ].canFind( r.front.toLower );
        if( !helpRequested && command )
        {
            this.parseImpl( r, syntax, commandPath ~ (*command).command );
            (*command).invoke();
            return;
        }
        else if( helpRequested )
        {
            if( command )
            {
                auto path = ( commandPath ~ (*command).command ).join( " " );
                if( syntax.commands.length )
                    stderr.writefln( "Usage: %s %s [<subcommand>] [<option>...]", this.appName, path );
                else
                    stderr.writefln( "Usage: %s %s [<option>...]", this.appName, path );

                stderr.writeln();
            }
            else if( syntax.commands.length == 0 )
            {
                stderr.writefln( "Usage: %s [<option>...]", this.appName );
                stderr.writeln();
            }
            else
            {
                stderr.writefln( "Usage: %s [<command>] [<option>...]", this.appName );
                stderr.writeln();
                stderr.writeln( "Available commands:" );
                stderr.writeln();
            }

            if( command )
            {
                if( syntax.commands.length )
                {
                    stderr.writefln( "Available subcommands for [%s]", (*command).command );
                    stderr.writeln();

                    foreach( name, command; syntax.commands )
                        stderr.writefln( "    %s - %s", name, command.helpText );

                    stderr.writeln();
                }
                stderr.writefln( "Available options for [%s]", (*command).command );
            }
           else
            {
                foreach( name, command; syntax.commands )
                    stderr.writefln( "    %s - %s", name, command.helpText );

                stderr.writeln();
                stderr.writeln( "Available options:" );
            }

            stderr.writeln();
            stderr.writeln( "    -h, -?, --help :: Show this help message" );
            foreach( option; syntax.options )
            {
                if( option.hasShortName )
                    stderr.writef( "    -%s, --%s :: %s", option.shortName, option.longName, option.helpText );
                else
                    stderr.writef( "    --%s :: %s", option.longName, option.helpText );

                if( option.required == Required.yes )
                    stderr.writeln( " [Required]" );
                else
                    stderr.writeln();
            }

            exit( int.min );
        }

        enum longRegex    = ctRegex!( "^--(?P<flag>.*?)(?:[=:](?P<value>.+))?$", "" );
        enum shortRegex   = ctRegex!( "^-(?P<flag>.)(?:[=:](?P<value>.+))?$", "" );
        enum bundledRegex = ctRegex!( "^-(?P<flags>.{2,})$", "" );

        string getNext( T )( TypeInfo type, T flag )
            if( isSomeChar!T || isSomeString!T )
        {
            if( r.empty )
            {
                if( type != typeid( bool ) )
                    throw new ArgumentParserException( "Option '%s' must have a value".format( flag ) );
                else
                    return "true";
            }

            auto current = r.front;
            if( current[0] != '-' )
            {
                r.popFront;
                return current;
            }
            else
            {
                if( type != typeid( bool ) )
                    throw new ArgumentParserException( "Option '%s' must have a value".format( flag ) );
                else
                    return "true";
            }
        }

        Option findOrElse( T )( T flag )
            if( isSomeChar!T || isSomeString!T )
        {
            Option result;
            if( !syntax.tryFind( flag, result ) )
            {
                if( syntax.config.ignoreUnrecognizedOptions == IgnoreUnrecognized.yes )
                    return null;
                else
                    throw new ArgumentParserException( "Unrecognized option '%s%s'".format( isSomeChar!T ? "-" : "--", flag ) );
            }

            return result;
        }

        while( !r.empty )
        {
            auto current = r.front; r.popFront;

            // Double dash by itself signals that we should stop parsing
            if( current == "--" )
                break;

            if( auto match = current.matchFirst( longRegex ) )
            {
                auto flag = match["flag"];
                auto option = findOrElse( flag );

                if( option is null )
                    continue;

                auto hasValue = match["value"] !is null && match["value"].length > 0;
                option = hasValue ? match["value"] : getNext( option.type, flag );
            }
            else if( auto match = current.matchFirst( shortRegex ) )
            {
                auto flag = match["flag"][0];
                auto option = findOrElse( flag );

                if( option is null )
                    continue;

                auto hasValue = match["value"] !is null && match["value"].length > 0;
                option = hasValue ? match["value"] : getNext( option.type, flag );
            }
            else if( auto match = current.matchFirst( bundledRegex ) )
            {
                foreach( flag; match["flags"] )
                {
                    auto option = findOrElse( flag );

                    if( option is null )
                        continue;

                    option = true;
                }
            }
        }

        auto notHandled = syntax.options.filter!( o => o.required == Required.yes );

        bool quit = false;
        foreach( opt; notHandled )
        {
            if( opt.handled )
                continue;

            quit = true;
            if( opt.hasShortName )
                stderr.writefln( "Missing required option -%s/--%s", opt.shortName, opt.longName );
            else
                stderr.writefln( "Missing required option --%s", opt.longName );
        }

        if( quit )
            exit( int.min );
    }
}

private TVal defaultParser( TVal )( string value )
    if( std.traits.isNumeric!TVal )
{
    return value.to!TVal;
}

private TVal defaultParser( TVal )( string value )
    if( is( TVal == bool ) )
{
    return !( [ "0", "no", "off", "false" ].canFind( value.strip.toLower ) );
}

private TVal defaultParser( TVal )( string value )
    if( is( TVal : string ) )
{
    return value;
}

unittest
{
    struct PersonOptions
    {
        string firstName;
        string lastName;
        ubyte age;
    }

    double test;
    bool verbose;
    PersonOptions person;
    void addEmployee()
    {
        assert( person != PersonOptions.init );
    }

    void testBuilder( ArgumentSyntax syntax )
    {
        syntax.config.caseSensitive = CaseSensitive.yes;
        syntax.config.allowBundling = AllowBundling.no;
        syntax.config.ignoreUnrecognizedOptions = IgnoreUnrecognized.no;

        syntax.command( "employee", "Employee operations", ( ArgumentSyntax syntax )
        {
            syntax.option( 't', "test", &test, Required.no, "Test option" );
            syntax.command( "new", &addEmployee, "Add new employee", ( ArgumentSyntax syntax )
            {
                syntax.option( "firstName", &person.firstName, Required.yes, "The employee's first name" );
                syntax.option( "lastName", &person.lastName, Required.yes, "The employee's last name" );
                syntax.option( 'a', "age", &person.age, Required.yes, "The employee's age" );
            } );
        } );

        syntax.option( 'v', "verbose", &verbose, Required.no, "Print extra information" );
    }

    void parse( string[] args )
    {
        ArgumentParser.parse( args, &testBuilder );
    }

    // $ manage -v no
    auto args1 = [
        "./manage.exe", // binary path. should always be first
        "-v", "no",
    ];

    // $ manage employee -t:123.45
    auto args2 = [
        "./manage.exe",
        "employee",
        "-t:123.45"
    ];

    // $ manage employee new --firstName John --lastName Doe --age 35
    auto args3 = [
        "./manage.exe",
        "employee",
        "new",
        "--firstName", "John",
        "--lastName", "Doe",
        "-a", "35"
    ];

    parse( args1 );
    parse( args2 );
    parse( args3 );

    assert( test == 123.45 );
    assert( verbose == false );
    assert( person == PersonOptions( "John", "Doe", 35 ) );
}
