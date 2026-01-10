------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       VIA RESTRICT BOARDS                                --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
--   to do:

with et_pcb_signal_layers;			use et_pcb_signal_layers;


package et_via_restrict.boards is

	use pac_geometry_2;

	procedure dummy;
	

	
	type type_via_restrict_contour is new -- CS rename to type_via_restrict_zone
		et_via_restrict.type_via_restrict_zone with
	record
		layers 	: pac_signal_layers.set;
	end record;
	
	package pac_via_restrict_contours is new doubly_linked_lists (type_via_restrict_contour);
	-- CS rename to pac_via_restrict_zones

	type type_via_restrict_cutout is new -- CS rename to type_via_restrict_zone_cutout
		et_via_restrict.type_via_restrict_cutout with
	record
		layers 	: pac_signal_layers.set;
	end record;
		
	package pac_via_restrict_cutouts is new doubly_linked_lists (type_via_restrict_cutout);
	-- CS rename to pac_via_restrict_zone_cutouts
	

	-- NOTE: 
	-- In the board drawing there is no "both-sides" as with
	-- silkscreen or assembly documentation. Here the signal
	-- layers specify which conductor layers are affected.
	
	type type_via_restrict is record
		contours	: pac_via_restrict_contours.list; -- CS rename contours to zone
		cutouts		: pac_via_restrict_cutouts.list;

		-- CS texts : 
		-- This must not be derived from from conductor text because
		-- it is not fabrication relevant.
		-- It should contain notes of the designer exclusively.
	end record;


	
	
end et_via_restrict.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
