------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PCB COORDINATES                               --
--                                                                          --
--                               B o d y                                    --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings;				use ada.strings;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with ada.tags;

with ada.exceptions;
with ada.numerics;
with ada.numerics.generic_elementary_functions;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;

package body et_pcb_coordinates is

	function to_string (face : in type_face) return string is begin
		return latin_1.space & to_lower (type_face'image (face));
	end to_string;

	function to_face (face : in string) return type_face is begin
		return type_face'value (face);
	end to_face;

	procedure flip (face : in out type_face) is begin
		case face is 
			when TOP => face := BOTTOM;
			when BOTTOM => face := TOP;
		end case;
	end flip;

	function to_string (p : in type_package_position) return string is begin
		return position_preamble
				& to_string (get_x (p))
				& axis_separator
				& to_string (get_y (p))
				& axis_separator
				& to_string (rot (p))
				& axis_separator
				& to_string (p.face);

	end to_string;
	
	function to_package_position (
		point 		: in type_point;
		rotation	: in type_rotation := zero_rotation;
		face		: in type_face := TOP)
		return type_package_position is
	begin
		return pos : type_package_position do
			set (pos, point);
			set (pos, rotation);
			pos.face := face;
		end return;
	end to_package_position;
	
	procedure set_face (
		face	: in type_face;
		position: in out type_package_position) is
	begin
		position.face := face;
	end set_face;

	function get_face (packge : in type_package_position)
		return type_face is
	begin
		return packge.face;
	end get_face;
	
	
	function to_terminal_position (
	-- Composes from a given point and angle the terminal position.
		point		: in type_point;
		rotation	: in type_rotation)
		return type_position'class is
		pos : type_position;
	begin
		--pos := (point with rotation);
		set (pos, point);
		set (pos, rotation);
		return pos;
	end to_terminal_position;
	
end et_pcb_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
