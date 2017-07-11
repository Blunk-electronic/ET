-- ---------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM M-1 STRING PROCESSING                          --
--                                                                          --
--                                 M-1                                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:

with ada.strings;				use ada.strings;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;

package body et_string_processing is

	function date_now return string is
		now		: time := clock;
		date	: string (1..19) := image(now, time_zone => utc_time_offset(now));
	begin
		return date;
	end date_now;
	
	function ht_to_space (c : in character) return character is
	begin 
		case c is
			when latin_1.ht => return latin_1.space;
			when others => return c;
		end case;
	end ht_to_space;		

	
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
	

	function remove_comment_from_line(text_in : string; comment_mark : in string) return string is
		position_of_comment : natural;
		-- NOTE: tabulators will be left unchanged. no substituion with whitespace is done !
	begin
		if text_in'length > 0 then -- if line contains something
			position_of_comment := index(text_in,comment_mark);
			case position_of_comment is -- check position of comment
				when 0 => -- no comment found -> return line as it is
					return text_in;
				when 1 => return ""; -- comment at beginning of line -> do nothing
				when others => -- comment somewhere in the line -> delete comment
					--put_line("comment at pos :" & natural'image(position_of_comment));
					return delete(text_in, position_of_comment, text_in'length); -- remove comment
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
	begin
		-- CS: do not strip anything if no quotes present
		-- if text_in(text_in'first) = latin_1.quote
		return text_in(text_in'first+1..text_in'last-1);
	end strip_quotes;

	function enclose_in_quotes (text_in : in string; quote : in character := latin_1.apostrophe) return string is
	-- Adds heading and trailing quotate to given string.
	begin
		return quote & text_in & quote;
	end enclose_in_quotes;

	function trim_space_in_string (text_in : in string) return string is
	-- shrinks successive space characters to a single one in given string		
		text_scratch : string (1..text_in'length) := text_in;
		
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

	function get_field_from_line( 
	-- Extracts a field separated by ifs at position. If trailer is true, the trailing content until trailer_to is also returned.
		text_in 	: in string;
		position 	: in positive;
		ifs 		: in character := latin_1.space;
		trailer 	: boolean := false;
		trailer_to 	: in character := latin_1.semicolon
		) return string is
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

	-- CS: comments
	function read_line ( 
	-- Breaks down a given string and returns a type_fields_of_line.
	-- CS: poor performance -> rework !
		line			: in string; -- the line to be broken down
		comment_mark	: in string; -- the comment mark like "--" or "#"
		ifs				: in character := latin_1.space -- field separator
		) return type_fields_of_line is
		list : type_list_of_strings.vector;
-- 		field_count : natural := ada.strings.fixed.count (line, ifs);

		procedure read_fields ( line : in string) is
			end_of_line : boolean := false;
			i : natural := 0;
		begin
-- 			put_line(line);
			while not end_of_line loop
				i := i + 1;
				if get_field_from_line(line, i, ifs)'last > 0 then
					type_list_of_strings.append(list, get_field_from_line(line, i, ifs));
				else
					end_of_line := true;
				end if;
			end loop;
		end read_fields;

	begin -- read_line
		-- If comment_mark is an empty string ("") no comments are to be removed (line remains unchanged).
		-- Otherwise the comment as specified by comment_mark is to be removed.
		if comment_mark'length = 0 then
			read_fields(line); -- no comment specified, leave line as it is
		else
			read_fields(remove_comment_from_line(line, comment_mark));
		end if;
		
		--1 + ada.strings.fixed.count(line,row_separator_1a);
-- 		type_list_of_strings.reserve_capacity(
-- 			list, 
-- 			count_type( ada.strings.fixed.count (line, ifs) ));
		
		return ( fields => list, field_count => type_list_of_strings.length(list));
	end read_line;

	function append (left : in type_fields_of_line; right : in type_fields_of_line) return type_fields_of_line is
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


	-- CS: comments
	function get_field_from_line (line : in type_fields_of_line; position : in positive) return string is
		use type_list_of_strings;
	begin
		if count_type(position) > line.field_count then
			return "";
		else
			return element(line.fields, positive(position));
		end if;
	end get_field_from_line;

	-- CS: comments	
	function to_string ( line : in type_fields_of_line) return string is
		s : unbounded_string;
		ifs : constant character := latin_1.space;
	begin
		if line.field_count > 0 then
			for i in 1..positive(line.field_count) loop
				case i is
					when 1 =>
						s := to_unbounded_string(trim(get_field_from_line(line, i),both));
					when others =>
						s := s & ifs & to_unbounded_string(trim(get_field_from_line(line, i),both));
				end case;
			end loop;
		end if;
		return to_string(s);
	end to_string;


	-- MESSAGES
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

	
end et_string_processing;

