-- ---------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           STRING PROCESSING                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:

with ada.strings;				use ada.strings;
with ada.strings.unbounded; 	use ada.strings.unbounded;

--with ada.exceptions;
with gnat.source_info;
with gnat.calendar;

package body et_string_processing is

	function to_string (
	-- Returns the given log level as string. 
		log_level	: in type_log_level;
		preamble	: in boolean := true) -- if true -> prepend preamble
		return string is
	begin
		if preamble then
			return "log level" & type_log_level'image (log_level);
		else
			return trim (type_log_level'image (log_level), left);
		end if;
	end to_string;
	
	procedure log_indentation_up is
	begin
		log_indentation := log_indentation + 1;
		exception
			when constraint_error =>
				put_line ("WARNING ! Maximum log indentation reached !");
				log_indentation := type_indentation_level'last;
			when others => null;
	end log_indentation_up;

	procedure log_indentation_down is
	begin
		log_indentation := log_indentation - 1;
		exception
			when constraint_error =>
				put_line ("WARNING ! Minimum log indentation reached !");
				log_indentation := type_indentation_level'first;
			when others => null;
	end log_indentation_down;

	procedure log_indentation_reset is
	begin
		log_indentation := type_indentation_level'first;
	end log_indentation_reset;

-- 	procedure log_indentation_operation (operation : in type_log_identation_operation) is
-- 	begin
-- 		null;
-- 	end log_indentation_operation;
	
	function indent (width : in type_indentation_level) return string is
	begin
		return (natural(width) * latin_1.space);
	end indent;

	procedure log (
		importance	: in type_message_importance := NORMAL;
		text		: in string;
		level		: in type_log_level := type_log_level'first;
		console		: in boolean := false) is

		function to_importance (importance : in type_message_importance) return string is begin
			case importance is
				when NORMAL => return "";
				when others => return type_message_importance'image (importance) & ": ";
			end case;
		end;
		
		function write_text (indentation_on : in boolean := true) 
			return string is 

			fill : string := natural (log_indentation) * latin_1.space;
		begin
			if indentation_on then
				return fill & to_importance (importance) & text;
			else
				return to_importance (importance) & text;
			end if;
		end;
		
	begin -- log
-- 		if level < no_logging then
			
			if log_level >= level then

				case importance is
					when NORMAL =>
						put_line (report_handle, write_text);

						if console then
							put_line (standard_output, write_text);
						end if;

					when NOTE =>
						put_line (report_handle, write_text);

						if console then
							put_line (standard_output, write_text);
						end if;

					when WARNING =>
						increment_warning_counter;
						
						put_line (report_handle, write_text (false)); -- indentation off

						if console then
							put_line (standard_output, write_text (false)); -- indentation off
						end if;

					when ERROR =>
						put_line (report_handle, write_text (false)); -- indentation off

						if console then
							put_line (standard_output, write_text (false)); -- indentation off
						end if;
						
				end case;
				
			end if;

-- 		end if;	
	end log;

	
	function date_now return type_date is
		now		: constant time := clock;
		date	: string (1..19) := image (now, time_zone => utc_time_offset (now));
	begin
		date (11) := 'T'; -- inserts a T so that the result is "2017-08-17T14:17:25"
		return type_date (date);
	end date_now;

	function date (preamble : in boolean := true) return string is
	-- Returns the current date as string in the format YYYY-MM-DDTHH:MM:SS
	begin
		if preamble then
			return "date " & string (date_now);
		else
			return string (date_now);
		end if;
	end date;

	function date_first return time is
		r : time := gnat.calendar.no_time; -- 1901-01-01
		--time_of (year => 1970, month => 01, day => 01, seconds => 1.0); -- return 1970-01-01
	begin
		return r;
	end date_first;
	
	function metric_system return string is
	-- Returns a message about the metric system used.
	begin
		return "CAUTION: Measurement system is METRIC. All dimensions given in millimeters !";
	end metric_system;

	function angles_in_degrees return string is
	-- Returns a message about the degrees used.
	begin
		return "CAUTION: All angles are given in degrees (1/360) !";
	end angles_in_degrees;	
	
	function message_warning return string is
	-- Returns a warning string and increments the warning counter.
		warning : constant string (1..9) := "WARNING #";
	begin
		increment_warning_counter;
		return warning & trim (warning_count, left) & " : ";
	end message_warning;

	function message_note return string is
	-- Returns a notification string.
	begin
		return "NOTE : ";
	end message_note;
	
-- 	procedure check_updated_vs_commissioned ( commissioned , updated : in type_date) is
-- 	-- Checks whether updated is later or equal commissioned.		
-- 	begin
-- 		if updated < commissioned then -- if updated before commissioned
-- 			write_message (
-- 				file_handle => current_output,
-- 				text => message_error & "The time of update is before the time of commission !"
-- 					& latin_1.lf 
-- 					& "commissioned : " & string(commissioned) & latin_1.lf
-- 					& "updated      : " & string(updated),
-- 					console => true);
-- 			
-- 			raise constraint_error;
-- 		end if;
-- 	end check_updated_vs_commissioned;

	
	function to_string (date : in type_date) return string is
	-- Returns the given date as string.
	begin
		return string (date);
	end to_string;
	
	function date_valid (date : in type_date) return boolean is
	-- Returns true if given date is valid and plausible.
	begin
		-- CS
		-- CS: call a procedure that says something like "date format invalid" or "date in stone age or date in future"
		return true;
	end date_valid;
	

	function strip_directory_separator (text : in string) return string is
	-- Removes the trailing directory separtor (if preset).
	begin
		if text (text'last) = '/' then -- CS: does not work with DOS/Windows
			return text (text'first .. text'last-1);
		else
			return text;
		end if;
	end strip_directory_separator;
	
	function ht_to_space (c : in character) return character is
	begin 
		case c is
			when latin_1.ht => return latin_1.space;
			when others => return c;
		end case;
	end ht_to_space;		

	function tilde_to_space (c : in character) return character is
	-- Replaces a tilde by space. Other characters are returned unchanged.
	begin
		case c is
			when '~' => return latin_1.space;
			when others => return c;
		end case;
	end tilde_to_space;						
	
	
	function wildcard_match (text_with_wildcards : in string; text_exact : in string) return boolean is
	-- Returns true if text_with_wildcards matches text_exact.
	-- text_with_wildcards is something like R41* , text_exact is something like R415
		count_asterisk		: natural := ada.strings.fixed.count(text_with_wildcards, 1 * latin_1.asterisk);
		count_question_mark	: natural := ada.strings.fixed.count(text_with_wildcards, 1 * latin_1.question);
		pos_asterisk		: natural := ada.strings.fixed.index(text_with_wildcards, 1 * latin_1.asterisk); -- first asterisk
		pos_question_mark	: natural := ada.strings.fixed.index(text_with_wildcards, 1 * latin_1.question); -- first question mark
		
		length_text_with_wildcards	: natural := text_with_wildcards'length;
		length_text_exact			: natural := text_exact'length;		
		
		match				: boolean := false;
	begin
		-- CS: zero-string length causes a no-match
		if length_text_exact = 0 or length_text_with_wildcards = 0 then
			return false;
		end if;
		
		-- CS: currently a question mark results in a no-match
		if count_question_mark > 0 then
			return false;
		end if;
		
		case count_asterisk is
			-- If no asterisks, texts must be equal in order to return a match:
			when 0 =>
				if length_text_exact = length_text_with_wildcards then
					if text_exact = text_with_wildcards then
						match := true;
					end if;
				end if;

			-- If one asterisk, compare left hand side of text_with_wildcards and text_exact:
			when 1 =>
				-- If text_exact is shorter than text_with_wildcards then we have no match.
				-- Example 1: text_exact is R41 and text_with_wildcards is R415*
				-- Example 2: text_exact is R41 and text_with_wildcards is R41*
				if length_text_exact < length_text_with_wildcards then
					match := false;
				elsif
				-- If text_exact and text_with_wildcards match from first character to pos_asterisk-1 we have a match.
				-- Example 1: text_exact is R415 and text_with_wildcards is R4*
					text_with_wildcards(text_with_wildcards'first .. text_with_wildcards'first - 1 + pos_asterisk - 1) = 
					text_exact         (text_exact'first          .. text_exact'first          - 1 + pos_asterisk - 1) then
					match := true;
-- 					put_line(standard_output,"match");
				end if;

			-- CS: currently more than one asterisk results in a no-match
			when others =>
				match := false;
		end case;
		
		return match;
	end wildcard_match;
	

	function remove_comment_from_line (
		text_in : in string;					-- the input string
		comment_mark : in string;				-- the comment mark (like "--" or "#"
		test_whole_line : in boolean := true	-- when false, cares for the comment mark at line begin only
												-- further comment marks are ignored
		) return string is
		position_of_comment : natural;
		-- NOTE: tabulators will be left unchanged. no substituion with whitespace is done !
	begin
		if text_in'length > 0 then -- if line contains something
			position_of_comment := index(text_in,comment_mark);
			case position_of_comment is -- check position of comment
				when 0 => -- no comment found -> return line as it is
					return text_in;
				when 1 => return ""; -- comment at beginning of line -> return empty string
				when others => -- comment somewhere in the line 
				
					if test_whole_line then --> delete comment
						return delete (text_in, position_of_comment, text_in'length); -- remove comment
					else
						return text_in; --> return line as it is
					end if;
			end case;
		end if;
		return "";
	end remove_comment_from_line;

	function get_field_count (text_in : string) return natural is
		line_length	:	Natural := text_in'last;	-- length of given text
		char_pt		:	Natural := 1;				-- charcter pointer (points to character being processed inside the given line)
		IFS1		: 	constant Character := ' '; 				-- field separator space
		IFS2		: 	constant Character := Character'Val(9); -- field separator tabulator
		field_ct	:	Natural := 0;				-- field counter (the first field found gets number 1 assigned)
		field_pt	:	Natural := 1;				-- field pointer (points to the charcter being processed inside the current field)
		inside_field:	Boolean := true;			-- true if char_pt points inside a field
		char_current:	Character;					-- holds current character being processed
		char_last	:	Character := ' ';			-- holds character processed previous to char_current
	begin
		while char_pt <= line_length
			loop
				--put (char_pt);
				char_current:= text_in(char_pt); 
				if char_current = IFS1 or char_current = IFS2 then
					inside_field := false;
				else
					inside_field := true;
				end if;

				-- count fields if character other than IFS found
				if ((char_last = IFS1 or char_last = IFS2) and (char_current /= IFS1 and char_current /= IFS2)) then
					field_ct:=field_ct+1;
				end if;

				-- save last character
				char_last:=char_current;
				-- advance character pointer by one
				char_pt:=char_pt+1; 
				--put (char_current); put (" --"); new_line;
			end loop;
		return field_ct;
	end get_field_count;



	function strip_quotes (text_in : in string) return string is
	-- removes heading and trailing quotation from given string
		quote : constant character := latin_1.quotation;
	begin
		-- if quote is first and last character
		if text_in (text_in'first) = quote and text_in (text_in'last) = quote then
			return text_in (text_in'first + 1 .. text_in'last - 1);
	
		-- if quote is first character
		elsif text_in (text_in'first) = quote then
			return text_in (text_in'first + 1 .. text_in'last);

		-- if quote is last character
		elsif text_in (text_in'last) = quote then
			return text_in (text_in'first .. text_in'last - 1);

		else
			return text_in;
		end if;
	end strip_quotes;

	function enclose_in_quotes (
		text_in	: in string;
		quote	: in character := latin_1.apostrophe) 
		return string is
	begin
		return quote & text_in & quote;
	end enclose_in_quotes;

	function enclose_in_quotes (
		charcter_in	: in character;
		quote		: in character := latin_1.apostrophe) 
		return string is
	begin
		return quote & charcter_in & quote;
	end enclose_in_quotes;
	
	function trim_space_in_string (text_in : in string) return string is
	-- shrinks successive space characters to a single one in given string		
		text_scratch : string (1..text_in'length) := text_in;

		universal_string_length_max	: constant natural := 1000;
		package type_universal_string is new generic_bounded_length(universal_string_length_max);
		use type_universal_string;
		
		s : type_universal_string.bounded_string; -- CS: might be not sufficient ! use type_long_string instead
		
		l : natural := text_scratch'length;
		sc : natural := natural'first;
	begin
		for c in 1..l loop
			case text_scratch(c) is
				when latin_1.space =>
					sc := sc + 1;
				when others =>
					if sc > 0 then
						s := append(left => s, right => latin_1.space);
					end if;
					s := append(left => s, right => text_scratch(c));
					sc := 0;
			end case;
		end loop;
		return to_string(s);
	end trim_space_in_string;

	function remove_trailing_directory_separator (path_in : string) return string is
	-- removes a trailing directory separator.
	begin
		if 	path_in (path_in'last) = '/' or -- on linux
			path_in (path_in'last) = '\' then -- on windows

			return path_in (path_in'first .. path_in'last - 1);
		else
			return path_in;
		end if;
	end remove_trailing_directory_separator;

		
	procedure write_message (
		file_handle : in ada.text_io.file_type;
		identation : in natural := 0;
		text : in string; 
		lf   : in boolean := true;		
		file : in boolean := true;
		console : in boolean := false) is
	begin
		if file then
			put(file_handle, identation * ' ' & text);
			if lf then 
				new_line(file_handle);
			end if;
		end if;

		if console then
			put(standard_output,identation * ' ' & text);
			if lf then 
				new_line(standard_output);
			end if;
		end if;
	end write_message;

	function is_number (text : in string) return boolean is
	-- Returns true if given string is a number. 
	-- CS: This test is very crude currently as it tests only the first character.
		first_character : constant character := text(text'first);
	begin
		if is_digit (first_character) then
			return true;
		else
			return false;
		end if;
	end;
	
	function get_field_from_line( 
	-- Extracts a field separated by ifs at position. If trailer is true, the trailing content until trailer_to is also returned.
		text_in 	: in string;
		position 	: in positive;
		ifs 		: in character := latin_1.space;
		trailer 	: in boolean := false;
		trailer_to 	: in character := latin_1.semicolon
		) return string is

		extended_string_length_max : constant natural := 1000; -- CS: increase if nessecary
		package type_extended_string is new generic_bounded_length (extended_string_length_max);
		use type_extended_string;
		
		field			: type_extended_string.bounded_string;	-- field content to return (NOTE: gets converted to string on return) 
		character_count	: natural := text_in'length;	-- number of characters in given string
		subtype type_character_pointer is natural range 0..character_count;
		char_pt			: type_character_pointer;		-- points to character being processed inside the given string
		field_ct		: natural := 0;					-- field counter (the first field found gets number 1 assigned)
		inside_field	: boolean := true;				-- true if char_pt points inside a field
		char_current	: character;					-- holds current character being processed
		char_last		: character := ifs;				-- holds character processed previous to char_current
	begin -- get_field
		--log ("get field from line " & text_in);
	
		if character_count > 0 then
			char_pt := 1;
			for char_pt in 1..character_count loop
			--while char_pt <= character_count loop
				char_current := text_in(char_pt); 
				
-- 				if char_current = ifs then
-- 					inside_field := false;
-- 				else
-- 					inside_field := true;
-- 				end if;

				-- CS: if ifs is space and fields are separated by a single ht, they are currently
				-- not split up. fix it !
				

				-- if ifs is space, then horizontal tabs must be threated equally
				if ifs = latin_1.space then
					if char_current = ifs or char_current = latin_1.ht then
						inside_field := false;
					else
						inside_field := true;
					end if;

					-- count fields if ifs is followed by a non-ifs character
					if (char_last = ifs or char_last = latin_1.ht) and (char_current /= ifs and char_current /= latin_1.ht) then
						field_ct := field_ct + 1;
					end if;
				else
					if char_current = ifs then
						inside_field := false;
					else
						inside_field := true;
					end if;

					-- count fields if ifs is followed by a non-ifs character
					if (char_last = ifs and char_current /= ifs) then
						field_ct := field_ct + 1;
					end if;
				end if;
				

-- 				-- count fields if ifs is followed by a non-ifs character
-- 				if (char_last = ifs and char_current /= ifs) then
-- 					field_ct := field_ct + 1;
-- 				end if;

				case trailer is
					when false =>
						-- if targeted field reached
						if position = field_ct then
							if inside_field then -- if inside field
								field := field & char_current; -- append current character to field
								--field_pt := field_pt + 1;
							end if;
						else
							-- if next field reached, abort and return field content
							if field_ct > position then 
									exit;
							end if;
						end if;

					when true =>
						-- if targeted field reached or passed
						if position <= field_ct then
							if char_current = trailer_to then
								exit;
							else
								field := field & char_current; -- append current character to field
							end if;
						end if;
				end case;

				-- save last character
				char_last := char_current;
			end loop;
		else
			null;
		end if;
		return to_string(field);
	end get_field_from_line;

	function read_line ( 
	-- Breaks down a given string and returns a type_fields_of_line.
		line			: in string; -- the line to be broken down
		number			: in positive_count := positive_count'first; -- the line number
		comment_mark	: in string; -- the comment mark like "--" or "#"
		test_whole_line	: in boolean := true; -- when false, cares for the comment mark at line begin only
												 -- further comment marks are ignored
		ifs				: in character := latin_1.space; -- field separator
		delimiter_wrap	: in boolean := false; -- true if text in delimiters is to be wrapped into a single field
		delimiter		: in character := latin_1.quotation -- the text delimiter sign (mostly ")
		) return type_fields_of_line is

		-- The list where we collect the fields contents.
		-- It MUST be a vector, because this allows do pick out arbitrary fields
		-- by their indexes.
		list : type_list_of_strings.vector;

		procedure read_fields (line : in string) is
		-- Breaks down the given line into smaller strings separated by ifs.
		-- Adds those smaller strings in container "list".
			field_start : positive := 1; -- temporarily storage of the position where a field starts
			field_entered : boolean := false; -- goes true once the first character of a field was found
			length : natural := line'length; -- the length of the given line

			-- As a safety measure, the pointer to the character being processed must be constrained
			-- so that it never becomes greater than the acutal length of the given line:
			subtype type_place is natural range 0..length;
			place : type_place := type_place'first; -- the character position being tested

			char : character; -- the character being tested

			-- The offset is used to determine the last character of a delimited field.
			-- For safety reasons it is constrained.
			-- The flag wrap_started goes true once a delimited field was found. It goes
			-- false when the delimited field ends.
			subtype type_offset is natural range 0..1;
			offset : type_offset := type_offset'first; 
			wrap_started : boolean := false; 

			-- CS: replace ht in given line by space
			
			procedure append (text_a : in string) is
			-- The given string text_a has a lower bound greater than zero.
			-- Convert the given string text_a to a string that has the lower bound of 1.
			-- Then append the new string to the list of strings.
				text_b : string (1..text_a'length) := text_a;
			begin
				type_list_of_strings.append (list, text_b);
			end append;

			function ifs_found return boolean is
			-- Tests if char is an ifs. Returns true in that case.
			-- If the given ifs is a space character,
			-- horizontal tabulators are threated like spaces.
			begin
				if ifs = latin_1.space then
					if char = ifs or char = latin_1.ht then -- threat space like horizontal tabulator
						return true;
					else
						return false;
					end if;
				else
					if char = ifs then
						return true;
					else
						return false;
					end if;
				end if;
			end ifs_found;
			
		begin -- read_fields
			-- If the given string "line" does not contain anything, there is nothing to do.
			-- Otherwise test each character in the line whether it is an ifs or field content.
			if line'length > 0 then
				--log ("line >" & line & "<");

				-- To make the reading of the code easier, we distinguish between processing a line
				-- that contains wrapped fields (delimiter_wrap cleared) and regular fields exclusively
				-- (delimiter_wrap set).

				-- PROCESS A LINE WITHOUT WRAPPED FIELDS.
				-- example: L P3V3 #PWR05
				if not delimiter_wrap then
					loop
						place := place + 1;
						char := line (place);

						if not field_entered then
							-- We are outside a field.
							if ifs_found then
								null; -- skip all ifs
							else -- field reached
								field_entered := true;
								field_start := place;
							end if;
						else
							-- We are inside a field. If an ifs is detected,
							-- the field is appended to the list.
							if ifs_found then
								field_entered := false;
								append (line (field_start..place-1));
							end if;
						end if;

						-- Exit loop on last character. If this is
						-- the last charcter of a field, append the field to list.
						if place = length then
							if field_entered then
								append (line (field_start..place));
							end if;
							exit;
						end if;
					end loop;

				-- PROCESS A LINE WITH WRAPPED FIELDS
				-- example: F 9 "PWR CTRL IN" H 1725 2950 51  0001 L BNN "purpose"
				else
					loop
						place := place + 1;
						char := line (place);

						if not field_entered then
							-- We are outside a field.
							if ifs_found then
								null; -- skip all ifs
							else -- field reached
								-- If a delimiter was found, signal that a wrapped field
								-- has started. Save the start position of the field content.
								-- The content starts right after the delimiter.
								-- If other charcter found, a regular field has started 
								-- where place is pointing at.
								-- In both cases a field has been entered.
								if char = delimiter then
									wrap_started := true; -- wrapped field has started
									field_start := place + 1; -- content right after the delimiter
								else
									field_start := place; -- regular field started
								end if;
	
								field_entered := true;
							end if;
						else
							-- We are inside a field. 
							-- If an ifs is detected and a wrapped field has started, the ifs is skipped
							-- because it is part of the wrapped field.
							-- If an ifs is detected and a regular field has started, then the regular
							-- field is appended to the list. The field started at field_start and ends
							-- at place - 1. Offset in this case is zero. 
							-- If a delimiter is detected, the wrapped field ends. Offset assumes 1 so 
							-- that on passing the ifs (right after delimiter) the last character position
							-- of the wrapped field can be computed.
							if ifs_found then
								if wrap_started then
									null; -- skip ifs
								else 
									field_entered := false;
									append (line (field_start..place - 1 - offset));
									offset := 0; -- reset offset for next wrapped field
								end if;
							else
								if char = delimiter then
									wrap_started := false;
									offset := 1;
								end if;
							end if;
						end if;

						-- Exit loop on last character. If this is
						-- the last charcter of a field, append the field to list.
						-- If last field was a wrapped field, the position of its last character
						-- is obtained by subtacting the offset (which is 1 in that case).
						-- Rais alarm on missing delimiter at end of line (flag wrap_started still set).
						if place = length then
							if field_entered then
								append (line (field_start..place - offset));
							end if;

							if wrap_started then
								log (ERROR, "missing delimiter " & delimiter & " at end of line !", console => true);
								log (text => "line: " & line, console => true);
								raise constraint_error;
							end if;
							
							exit;
						end if;
						
					end loop;

				end if;
			end if;
		end read_fields;

	begin -- read_line
		-- If comment_mark is an empty string ("") no comments are to be removed (line remains unchanged).
		-- Otherwise the comment as specified by comment_mark is to be removed.
		if comment_mark'length = 0 then
			read_fields (line); -- no comment specified, leave line as it is
		else
			read_fields (remove_comment_from_line (
				text_in => line,
				comment_mark => comment_mark,
				test_whole_line => test_whole_line));
		end if;

		return (
			fields => list,
			field_count => type_list_of_strings.length (list),
			number => number);
	end read_line;

	function append (
		left	: in type_fields_of_line;
		right	: in type_fields_of_line)
		return type_fields_of_line 
	is		
		line : type_fields_of_line;
		use type_list_of_strings;
	begin
--		line.fields := fields);
--		line.field_count := 0;

		line.fields := left.fields & right.fields;
		line.field_count := left.field_count + right.field_count;
-- 		if right.field_count > 0 then
-- 			null;
-- 		end if;
		return line;
	end append;

	-- Remove fields from line:
	function remove (
		line	: in type_fields_of_line;
		first	: in positive;
		last	: in positive) 
		return type_fields_of_line
	is
		use type_list_of_strings;
		result : type_fields_of_line;
	begin
		-- Iterate all fields of given line:
		for f in first_index (line.fields) .. last_index (line.fields) loop

			-- Skip fields in given range. All other fields are appended to result.fields:
			if f < first or f > last then
				append (result.fields, element (line.fields, f));
				result.field_count := result.field_count + 1;
			end if;
		end loop;
		
		return result;
	end remove;
	
	procedure set_field (
		line		: in out type_fields_of_line;
		position	: in positive;
		content		: in string)
	is begin
		null; -- CS
	end set_field;
	
	function field (line : in type_fields_of_line; position : in positive) return string is
	-- Returns the field at the given position. Raises constraint error if there is no 
	-- field at given position.		
		use type_list_of_strings;
	begin
		--log ("field ct:" & count_type'image (line.field_count));
		if count_type (position) > line.field_count then
			--return "";
			raise constraint_error;
		else
			return element (line.fields, positive (position));
		end if;
	end field;

	-- CS: comments	
	function to_string (line : in type_fields_of_line) return string is
		s : unbounded_string;
		ifs : constant character := latin_1.space;
	begin
		if line.field_count > 0 then
			for i in 1..positive (line.field_count) loop
				case i is
					when 1 =>
						s := to_unbounded_string (trim (field (line, i),both));
					when others =>
						s := s & ifs & to_unbounded_string (trim (field (line, i),both));
				end case;
			end loop;
		end if;
		return to_string (s);
	end to_string;

	function line_number (line : in type_fields_of_line) return positive is
	-- Returns the line number of the given line.
	begin
		return positive (line.number);
	end line_number;
	
	function affected_line (line : in type_fields_of_line ) return string is
	-- Returns the line number of the given line in a string like "line x:"
	begin
		return ("line" & positive_count'image (line.number) & ": ");
	end affected_line;

	function field_count (line : in type_fields_of_line) return count_type is
	-- Returns the number of fields in the given line.
	begin
		return line.field_count;
	end field_count;

	function lines_equally (left, right : in type_fields_of_line) return boolean is
		use type_list_of_strings;
	begin
		-- compare field counts
		if left.field_count /= right.field_count then
			return false;
		end if;

		-- compare line numbers
		if left.number /= right.number then
			return false;
		end if;

		-- compare fields contents
		if left.fields /= right.fields then
			return false;
		end if;
		
		return true;
	end lines_equally;

	procedure increment_warning_counter is begin
	-- Increments the warning counter by one.
		warning_counter := warning_counter + 1;
	end increment_warning_counter;

	function warning_count return type_warning_counter is begin
	-- Returns the number of warnings.
		return warning_counter;
	end warning_count;

	function no_warnings return boolean is begin
	-- Returns true if no warnings have been generated.
		if warning_counter = 0 then return true;
		else return false;
		end if;
	end no_warnings;
	
	function warning_count return string is begin
	-- Returns the number of warnings as string.
		return type_warning_counter'image (warning_counter);
	end warning_count;

	function log_file_name return string is
	-- Returns the relative path and name of the import report file.
		use et_general;
	begin
		return compose ( 
			containing_directory 	=> compose (work_directory, report_directory),
			name					=> "messages",
			extension				=> report_extension
			);
	end log_file_name;

	procedure create_report is
	-- Creates the report file in report_directory.
		use et_general;
		previous_output : ada.text_io.file_type renames current_output;
    begin
		create (file => report_handle,
				mode => out_file, 
				name => log_file_name);

		set_output (report_handle);
		
		put_line (system_name & " " & version & " messages log");
		put_line (date);
		put_line (metric_system);
		put_line (angles_in_degrees);
		put_line (row_separator_double);

		set_output (previous_output);
	end create_report;

	procedure close_report is
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.
		use et_general;
	begin
		if is_open (report_handle) then

			set_output (report_handle);
			
			put_line (row_separator_double);

			if no_warnings then
				put_line ("no warnings");
			else
				put_line ("warnings" & warning_count);
			end if;
			
			put_line (row_separator_single);
			
			put_line (date);
			put_line (system_name & " log messages end");

			set_output (standard_output);
			
			close (report_handle);

			if not no_warnings then -- means if there are warnings
				put_line (standard_output, "WARNING ! "
					& "Read log file " & log_file_name & " for warnings and error messages !");
			end if;
			
		end if;
	end close_report;


	procedure show_line (
	-- Output the line of code where the exception occured:
		file : string; -- the file name like et_kicad.adb
		line : natural) is -- the line number 
	begin
		log_indentation_reset;
		log (text => "source file " & file & " line" & natural'image (line), console => true);
	end;

	
end et_string_processing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
