------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GENERAL_RW                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.exceptions;

with et_string_processing;

package body general_rw is
	
	-- This function returns the string at position in given line:
	-- It is frequently used when reading lines of files.
	function f (line : in et_string_processing.type_fields_of_line; position : in positive) return string 
		renames et_string_processing.field;

	
	procedure expect_field_count (
		line			: in et_string_processing.type_fields_of_line;	-- the list of fields of the line
		count_expected	: in count_type;			-- the min. number of fields to expect
		warn			: in boolean := true) 		-- warn if too many fields
		is 
		use et_string_processing;
		count_found : constant count_type := field_count (line);

		f1 : string := f (line, 1); -- CS: line must have at least one field otherwise exception occurs here
	begin
		if count_found = count_expected then null; -- fine, field count as expected
		
		elsif count_found < count_expected then -- less fields than expected
			log (ERROR, "missing parameter for '" & f1 & "' !", console => true);
			raise constraint_error;
			
		elsif count_found > count_expected then -- more fields than expeced
			if warn then
				log (WARNING, affected_line (line) & "excessive parameters after '" &
					f (line, positive (count_expected)) & "' ignored !");
			end if;
		end if;
		
	end expect_field_count;

	procedure invalid_keyword (word : in string) is 
		use et_string_processing;
	begin
		log (ERROR, "invalid keyword '" & word & "' !", console => true);
		raise constraint_error;
	end;


-- SECTIONS AND INDENTATION
	function write_top_level_reached return string is begin return "top level reached"; end;
	function write_enter_section return string is begin return "entering section "; end;
	function write_return_to_section return string is begin return "returning to section "; end;

	function write_missing_begin_end return string is begin 
		return "missing " & section_begin & " or " & section_end & " after section name !"; end;

	function write_section_stack_not_empty return string is begin
		return "section stack not empty !"; end;
	
	procedure invalid_section is 
		use et_string_processing;
	begin
		log (ERROR, "invalid section name !", console => true);
		raise constraint_error;
	end;
	
	procedure tab_depth_up is begin tab_depth := tab_depth + 1; end tab_depth_up;
	procedure tab_depth_down is begin tab_depth := tab_depth - 1; end tab_depth_down;
	procedure reset_tab_depth is begin tab_depth := type_tab_depth'first; end reset_tab_depth;

	procedure section_mark (section : in string; mark : in type_section_mark) is begin
	-- Make sure the current_output is set properly.
		case mark is
			when HEADER =>
				--new_line;
				put_line (tab_depth * tab & section & space & section_begin);
				tab_depth_up;
			when FOOTER =>
				tab_depth_down;
				put_line (tab_depth * tab & section & space & section_end);
		end case;
	end section_mark;

	procedure line_begin is begin section_mark (section_line, HEADER); end;
	procedure line_end   is begin section_mark (section_line, FOOTER); end;			
	procedure arc_begin  is begin section_mark (section_arc , HEADER); end;
	procedure arc_end    is begin section_mark (section_arc , FOOTER); end;
	procedure circle_begin is begin section_mark (section_circle, HEADER); end;
	procedure circle_end   is begin section_mark (section_circle, FOOTER); end;			

	procedure text_begin is begin section_mark (section_text, HEADER); end;
	procedure text_end   is begin section_mark (section_text, FOOTER); end;
	procedure placeholder_begin is begin section_mark (section_placeholder, HEADER); end;
	procedure placeholder_end   is begin section_mark (section_placeholder, FOOTER); end;
	
	procedure write (
		keyword 	: in string;
		parameters	: in string;
		space 		: in boolean := false;
		wrap		: in boolean := false) is
		use latin_1;
		parameters_wrapped : string (1..parameters'length + 2);
	begin -- write
		if wrap then
			parameters_wrapped := quotation & parameters & quotation;
		end if;
					
		if wrap then
			-- If wrapping required, a space is always between keyword and parameters
			put_line (tab_depth * tab & keyword & latin_1.space & parameters_wrapped);
		else
-- 			case space is
-- 				when true =>
					put_line (tab_depth * tab & keyword & latin_1.space & parameters);
-- 				when false =>
-- 					put_line (tab_depth * tab & keyword & parameters);
-- 			end case;
		end if;
	end write;	



	
-- GENERICS
	
	package body stack_lifo is
		s : array (1..max) of item;
		top : natural range 0..max;

		procedure push (x : in item) is
		begin
			top := top + 1;
			s (top) := x;
		end push;

		procedure pop is
		begin
			top := top - 1;
		end pop;
		
		function pop return item is
		begin
			top := top - 1;
			return s (top + 1);
		end pop;

		function depth return natural is
		begin
			return top;
		end depth;

		procedure init is
		begin
			top := 0;
		end init;

		function empty return boolean is
		begin
			if top = 0 then return true;
			else return false;
			end if;
		end empty;
		
		function current return item is 
		begin
			return s (top);
		end current;
		
		function parent (degree : in natural := 1) return item is
		begin
			--return s (top - 1);
			return s (top - degree);
		end parent;
		
	end stack_lifo;

	
end general_rw;
