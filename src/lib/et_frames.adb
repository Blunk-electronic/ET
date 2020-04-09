------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               FRAMES                                     --
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

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;

package body et_frames is

	function to_paper_size (paper_size : in string) return type_paper_size is begin
		return type_paper_size'value (paper_size);
	end;
	
	function to_string (paper_size : in type_paper_size) return string is begin
		return type_paper_size'image (paper_size);
	end;

	
	function to_string (orientation : in type_orientation) return string is begin
		return to_lower (type_orientation'image (orientation));
	end;

	function to_orientation (orientation : in string) return type_orientation is begin
		return type_orientation'value (orientation);
	end;


	function to_string (rows : in type_rows) return string is begin
		return trim (type_rows'image (rows), left);
	end;

	function to_rows (rows : in string) return type_rows is begin
		return type_rows'value (rows);
	end;

	function to_string (columns : in type_columns) return string is begin
		return trim (type_columns'image (columns), left);
	end;

	function to_columns (columns : in string) return type_columns is begin
		return type_columns'value (columns);
	end;


	

	function to_string (distance : in type_distance) return string is begin
		return trim (type_distance'image (distance), left);
	end;

	function to_distance (distance : in string) return type_distance is begin
		return type_distance'value (distance);
	end;

	

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the corresponding size in mm.
		paper_size	: in type_paper_size;
		orientation	: in type_orientation := LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance is

		dimension : type_distance;
	
	begin
		case orientation is
			when LANDSCAPE =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_x;
							when Y => dimension := paper_size_A3_y;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_x;
							when Y => dimension := paper_size_A4_y;
						end case;
				end case;

			when PORTRAIT =>
				case paper_size is 
					when A3 =>
						case axis is
							when X => dimension := paper_size_A3_y;
							when Y => dimension := paper_size_A3_x;
						end case;

					when A4 =>
						case axis is
							when X => dimension := paper_size_A4_y;
							when Y => dimension := paper_size_A4_x;
						end case;
				end case;

		end case;

		return dimension;
	end paper_dimension;




	function to_string (name : in pac_template_name.bounded_string) return string is begin
		return pac_template_name.to_string (name);
	end;
	
	function to_template_name (name : in string) return pac_template_name.bounded_string is begin
		return pac_template_name.to_bounded_string (name);
	end;

	function to_string (domain : in type_domain) return string is begin
		return to_lower (type_domain'image (domain));
	end;

	function to_domain (domain : in string) return type_domain is begin
		return type_domain'value (domain);
	end;
	

	function to_string (cat : in type_schematic_sheet_category) return string is begin
		return type_schematic_sheet_category'image (cat);
	end;

	function to_category (cat : in string) return type_schematic_sheet_category is begin
		return type_schematic_sheet_category'value (cat);
	end;
	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
