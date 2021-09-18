------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD SHAPES AND TEXT                              --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:

with ada.characters.handling;	use ada.characters.handling;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;

package body et_board_shapes_and_text is
	
	procedure validate_general_line_width (width : in type_distance) is
	begin
		if width not in type_general_line_width then
			log (ERROR, "line width invalid ! Allowed range is" 
				 & to_string (type_general_line_width'first) & " .."
				 & to_string (type_general_line_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_general_line_width;


	-- EASING
	function to_easing_style (easing : in string) return type_easing_style is begin
		return type_easing_style'value (easing);
	end;

	function to_string (easing : in type_easing_style) return string is begin
		return to_lower (type_easing_style'image (easing));
	end;

	

	function to_string (circle : in type_fillable_circle) return string is begin
		case circle.filled is
			when NO =>
				return
					pac_shapes.to_string (type_circle (circle)) &
					latin_1.space & et_text.keyword_line_width & to_string (circle.border_width);

			when YES =>
				case circle.fill_style is
					when SOLID =>
						return 
							pac_shapes.to_string (type_circle (circle)) &
							latin_1.space & keyword_fill_style & latin_1.space & to_string (circle.fill_style);

					when HATCHED =>
						return
							pac_shapes.to_string (type_circle (circle)) &
							latin_1.space & keyword_fill_style & latin_1.space & to_string (circle.fill_style) &
							latin_1.space & keyword_hatching_line_width & to_string (circle.hatching.line_width) &
							latin_1.space & keyword_hatching_line_spacing & to_string (circle.hatching.spacing);
				end case;
		end case;
	end;


	
	
end et_board_shapes_and_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
