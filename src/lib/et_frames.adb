------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               FRAMES                                     --
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

package body et_frames is

	function to_paper_size (paper_size : in string) return type_paper_size is begin
		return type_paper_size'value (paper_size);
	end;
	
	function to_string (paper_size : in type_paper_size) return string is begin
		return type_paper_size'image (paper_size);
	end;
	


	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in type_paper_size;
		orientation	: in type_paper_orientation := LANDSCAPE;
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


-- SCHEMATIC

	function to_string (name : in pac_schematic_template_name.bounded_string) return string is begin
		return pac_schematic_template_name.to_string (name);
	end;
	
	function to_template_name (name : in string) return pac_schematic_template_name.bounded_string is begin
		return pac_schematic_template_name.to_bounded_string (name);
	end;


-- PCB RELATED

	function to_string (name : in pac_pcb_template_name.bounded_string) return string is begin
		return pac_pcb_template_name.to_string (name);
	end;
	
	function to_template_name (name : in string) return pac_pcb_template_name.bounded_string is begin
		return pac_pcb_template_name.to_bounded_string (name);
	end;
	
	
end et_frames;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
