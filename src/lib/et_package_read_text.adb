------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       PACKAGE READ / TEXT                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;						use ada.text_io;
with ada.strings; 						use ada.strings;

with et_design_rules_board;				use et_design_rules_board;
with et_board_geometry;					use et_board_geometry;

with et_alignment;						use et_alignment;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_keywords;						use et_keywords;
with et_package_model;					use et_package_model;
with et_text_content;					use et_text_content;
with et_device_placeholders;			use et_device_placeholders;
with et_general_rw;						use et_general_rw;


package body et_package_read_text is

	use pac_geometry_2;	
	
	


	procedure read_text (
		line : in type_fields_of_line)
	is	
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position of note starting at field 2
			pac_text.position := to_position (line, 2);

		elsif kw = keyword_size then -- size 1.000
			expect_field_count (line, 2);
			pac_text.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.1
			expect_field_count (line, 2);
			pac_text.line_width := to_distance (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			pac_text.alignment := to_alignment (line, 2);
			
		elsif kw = keyword_content then -- content "keep clear"
			expect_field_count (line, 2); -- actual content in quotes !
			pac_text.content := to_content (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_text;





		
	procedure read_placeholder (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position of note starting at field 2
			pac_text_placeholder.position := to_position (line, 2);

		elsif kw = keyword_size then -- size 1.000
			expect_field_count (line, 2);
			pac_text_placeholder.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.1
			expect_field_count (line, 2);
			pac_text_placeholder.line_width := to_distance (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			pac_text_placeholder.alignment := to_alignment (line, 2);
			
		elsif kw = keyword_meaning then -- meaning reference, value, purpose
			expect_field_count (line, 2);
			pac_text_placeholder.meaning := to_meaning (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_placeholder;
	

	
end et_package_read_text;
