------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET GENERAL DECLARATIONS                        --
--                                                                          --
--                                 ET                                       --
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
--
with ada.strings.unbounded; 	use ada.strings.unbounded;

package body et_general is

	--- STRING PROCESSING
	
	function get_field
	-- Extracts a field separated by ifs at position. If trailer is true, the trailing content until trailer_to is also returned.
			(
			text_in 	: in string;
			position 	: in positive;
			ifs 		: in character := latin_1.space;
			trailer 	: boolean := false;
			trailer_to 	: in character := latin_1.semicolon
			) return string is
		field			: unbounded_string;				-- field content to return (NOTE: gets converted to string on return) -- CS: use bounded string
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

				-- if ifs is space, then horizontal tabs must be threated equally
				if ifs = latin_1.space then
					if char_current = ifs or char_current = latin_1.ht then
						inside_field := false;
					else
						inside_field := true;
					end if;
				else
					if char_current = ifs then
						inside_field := false;
					else
						inside_field := true;
					end if;
				end if;
				
				-- count fields if ifs is followed by a non-ifs character
				if (char_last = ifs and char_current /= ifs) then
					field_ct := field_ct + 1;
				end if;

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
	end get_field;


	function strip_quotes (text_in : in string) return string is
	-- removes heading and trailing quotation from given string		
	begin
		return text_in(text_in'first+1..text_in'last-1);
	end strip_quotes;

	
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
			put(identation * ' ' & text);
			if lf then 
				new_line;
			end if;
		end if;
	end write_message;

	

	-- GENERICS
	
	package body stack_lifo is
		s : array (1..max) of item;
		top : natural range 0..max;

		procedure push(x : item) is
		begin
			top := top + 1;
			s(top) := x;
		end push;

		function pop return item is
		begin
			top := top - 1;
			return s(top + 1);
		end pop;

		function depth return natural is
		begin
			return top;
		end depth;

		procedure init is
		begin
			top := 0;
		end init;
	
	end stack_lifo;

end et_general;

