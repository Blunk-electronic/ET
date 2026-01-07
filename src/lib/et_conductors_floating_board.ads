------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    FLOATING CONDUCTORS / BOARD                           --
--                                                                          --
--                               S p e c                                    --
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
--   to do:
--

with et_conductor_segment.boards;		use et_conductor_segment.boards;
with et_conductor_text.boards;			use et_conductor_text.boards;
with et_fill_zones.boards;				use et_fill_zones.boards;
with et_pcb_placeholders.conductor;		use et_pcb_placeholders.conductor;


package et_conductors_floating_board is

	
	-- Type for NON ELECTRIC !! conductor objects.
	-- All these objects are not connected to any net,
	-- means they are floating.
	-- NON ELECTRIC conductor objects of a pcb may also 
	-- include text placeholders:
	type type_conductors_floating is record
		lines 			: pac_conductor_lines.list;
		arcs			: pac_conductor_arcs.list;
		circles			: pac_conductor_circles.list;

		-- floating fill zones:
		zones			: type_floating;
		-- Useful to catch the liquid solder during wave soldering ?

		-- global cutout areas:
		cutouts			: pac_cutouts.list;
		
		texts			: pac_conductor_texts_board.list;
		placeholders	: pac_placeholders_conductor.list;
	end record;


	-- CS procedures add_line, add_arc, add_circle, add_zone, add_text, add_placeholder

	
end et_conductors_floating_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
