------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD SHAPES AND TEXT                              --
--                                                                          --
--                              S p e c                                     --
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


with et_text;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;

package et_board_shapes_and_text is
	use pac_geometry_brd;

	-- Instantiation of the shapes package:
	package pac_shapes is new 
		et_geometry.generic_pac_shapes (et_pcb_coordinates.pac_geometry_brd);


	type type_text_parameters is record
		size_min 		: type_distance_positive;
		size_max 		: type_distance_positive;
		size_default 	: type_distance_positive;		
		width_min 		: type_distance_positive;
		width_max 		: type_distance_positive;
		width_default 	: type_distance_positive;
	end record;


	-- FAB RELEVANT
	text_parameters_fab : constant type_text_parameters := (
		size_min 		=> 0.5,
		size_max 		=> 100.0,
		size_default 	=> 1.5,
		width_min 		=> 0.15,
		width_max 		=> 10.0,
		width_default 	=> 0.15);

	package pac_text_fab is new et_text.generic_pac_text (
		pac_shapes			=> pac_shapes,
		size_min			=> text_parameters_fab.size_min,
		size_max			=> text_parameters_fab.size_max,
		size_default		=> text_parameters_fab.size_default,
		line_width_min		=> text_parameters_fab.width_min,
		line_width_max		=> text_parameters_fab.width_max,
		line_width_default	=> text_parameters_fab.width_default);


	-- DOCUMENTATION RELEVANT (NON-FAB)
	text_parameters_doc : constant type_text_parameters := (
		size_min 		=> 0.1,
		size_max 		=> 100.0,
		size_default 	=> 1.5,
		width_min 		=> 0.05,
		width_max 		=> 10.0,
		width_default 	=> 0.05);
	
	package pac_text_doc is new et_text.generic_pac_text (
		pac_shapes			=> pac_shapes,
		size_min			=> text_parameters_doc.size_min,
		size_max			=> text_parameters_doc.size_max,
		size_default		=> text_parameters_doc.size_default,
		line_width_min		=> text_parameters_doc.width_min,
		line_width_max		=> text_parameters_doc.width_max,
		line_width_default	=> text_parameters_doc.width_default);
	
end et_board_shapes_and_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
