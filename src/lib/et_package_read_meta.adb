------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       PACKAGE READ / META                                --
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
--
-- DESCRIPTION:
-- 
-- This is about general meta data of a package.
--
--   do do:
--
--

with ada.text_io;						use ada.text_io;
with ada.strings; 						use ada.strings;

with et_design_rules_board;				use et_design_rules_board;

with et_keywords;						use et_keywords;
with et_package_model;					use et_package_model;

with et_general_rw;						use et_general_rw;



package body et_package_read_meta is



	procedure read_meta (
		line 			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_bom_relevant then -- bom_relevant yes/no
			expect_field_count (line, 2);
			pac_appearance := to_bom_relevant (f (line,2));

			-- Depending on the appearance we create a virtual or real package
			-- where pointer packge is pointing at:
			case pac_appearance is
				when BOM_RELEVANT_YES =>
					packge := new type_package_model' (
								appearance	=> BOM_RELEVANT_YES,
								others		=> <>);

				when BOM_RELEVANT_NO =>
					packge := new type_package_model' (
								appearance	=> BOM_RELEVANT_NO,
								others		=> <>);
			end case;
					
		elsif kw = keyword_description then -- description "blabla"
			expect_field_count (line, 2);
			pac_description := to_package_description (f (line,2));

		elsif kw = keyword_assembly_technology then -- technology SMT/THT
			expect_field_count (line, 2);
			pac_technology := to_assembly_technology (f (line,2));
			
		else
			invalid_keyword (kw);
		end if;
	end;

	


	procedure assign_meta (
		packge			: in type_package_model_access;
		log_threshold	: in type_log_level)
	is begin
		packge.description := pac_description;
		packge.technology := pac_technology;
	end assign_meta;

	
	
end et_package_read_meta;
