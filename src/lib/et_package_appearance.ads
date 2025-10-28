------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PACKAGE APPEARANCE                               --
--                                                                          --
--                               S p e c                                    --
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
--                                                                          --
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
-- DESCRIPTION:
--
-- Usually a device has a package (or a housing or a case).
-- Other CAE tools use the term "footprint".
-- There are devices which have a physical representation
-- in the board drawing but they are not mountable, they can not be
-- assembled because:
--
-- 1. they exist just because of a certain shape of the PCB.
--    Examples: edge connectors, mounting holes
--
-- 2. they consist of conductors (copper structures) and some
--    silkscreen. Examples: Testpoints, Conductor pads where spring
--    connectors are hooked up.
--
-- 3. they do not have a height. Means there is no z-component.
--
-- The fact that a packge has no real device implies that such a 
-- device never appears in material lists (BOM - Bill Of Material).
--
-- So in this package the types and subprograms required to
-- distinguish between real packages and virtual packages are
-- provided.


package et_package_appearance is

	keyword_bom_relevant : constant string := "bom_relevant";
	

	bom_relevant_prefix : constant string := "BOM_RELEVANT_";

	
	type type_bom_relevant is (
								  
		-- Real packages with x,y,z dimension:
		BOM_RELEVANT_YES,	

		-- For packages that do not appear in
		-- material lists, which do not have a real package 
		-- (like testpoints, edge connectors, 
		-- mounting holes, fiducials, ...):
		BOM_RELEVANT_NO);	

	
	
	bom_relevant_default : constant type_bom_relevant := BOM_RELEVANT_YES;

	
	function to_string (
		bom_relevant : in type_bom_relevant) 
		return string;

	
	function to_bom_relevant (
		bom_relevant : in string) 
		return type_bom_relevant;

	
	
end et_package_appearance;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
