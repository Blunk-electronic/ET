------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PCB DESIGN RULES                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
--   ToDo:


with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.directories;				use ada.directories;

with et_keywords;					use et_keywords;
with et_string_processing;			use et_string_processing;
with et_exceptions;					use et_exceptions;


package body et_design_rules_board is


	function is_empty (rules : in type_design_rules_file_name)
		return boolean
	is (pac_file_name.length (pac_file_name.bounded_string (rules)) = 0);



	function to_file_name (file : in string)
		return type_design_rules_file_name
	is (type_design_rules_file_name (pac_file_name.to_bounded_string (file)));



	function to_string (file : in type_design_rules_file_name)
		return string
	is (pac_file_name.to_string (pac_file_name.bounded_string (file)));



	procedure validate_track_clearance (clearance : in type_distance_model) is begin
		if clearance not in type_track_clearance then
			raise semantic_error_1 with
				"ERROR: Track clearance invalid ! Allowed range is"
				 & to_string (type_track_clearance'first) & " .."
				 & to_string (type_track_clearance'last);
		end if;
	end validate_track_clearance;






	function auto_set_restring (
		category	: in type_restring_category;
		drill_size	: in type_drill_size;
		delta_size	: in type_restring_delta_inner_outer := zero)
		return type_restring_width
	is
		result : type_restring_width;
		scratch : type_distance_positive;
	begin
		--scratch := drill_size + drill_size * drill_to_restring_multiplier;
		scratch := drill_size * drill_to_restring_multiplier;

		case category is
			when OUTER =>
				result := scratch;

			when INNER =>
				result := delta_size + scratch;
		end case;

		return result;
	end auto_set_restring;




	procedure validate_restring_width (
		restring_width : in type_distance_model)
	is begin
		if restring_width not in type_restring_width then
			raise semantic_error_1 with
				"ERROR: Restring width invalid ! Allowed range is"
				 & to_string (type_restring_width'first) & " .."
				 & to_string (type_restring_width'last);
		end if;
	end validate_restring_width;




	procedure validate_track_width (
		track_width : in type_distance_model)
	is begin
		if track_width not in type_track_width then
			raise semantic_error_1 with
				"ERROR: Track width invalid ! Allowed range is"
				 & to_string (type_track_width'first) & " .."
				 & to_string (type_track_width'last);
		end if;
	end validate_track_width;




	procedure read_rules (
		file_name		: in type_design_rules_file_name;
		log_threshold	: in type_log_level)
	is separate;



	function get_rules (rules : in type_design_rules_file_name) -- JLP_ML4_standard.dru
		return type_design_rules_board
	is
		use pac_design_rules_board;

		-- Locate the design rules:
		c : constant pac_design_rules_board.cursor := find (design_rules, rules);
	begin
		-- If the given rules file does not exist (of if rules is empty)
		-- return default rules:
		if c /= no_element then
			return element (c);
		else
			return design_rules_default;
		end if;
	end get_rules;

end et_design_rules_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
