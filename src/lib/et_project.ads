------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET PROJECT                               --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your editor to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_coordinates;			use et_coordinates;
with et_libraries;				use et_libraries;
with et_string_processing;


package et_project is

	-- after importing a foreign project, native ET projects are created here:
	directory_import		: constant string (1..9)	:= "et_import";

	-- native ET projects live in a parent folder
	directory_projects		: constant string (1..8)	:= "projects";

	-- native ET libraries live in a parent folder
	directory_libraries		: constant string (1..3)	:= "lbr";

	-- supplementary stuff of a project
	directory_dru			: constant string (1..12)	:= "design_rules";
	directory_cam			: constant string (1..3)	:= "CAM";
	directory_net_classes	: constant string (1..11)	:= "net_classes";	
	directory_settings		: constant string (1..8)	:= "settings";
	directory_reports		: constant string (1..7)	:= "reports";
	directory_documentation	: constant string (1..13)	:= "documentation";
	directory_miscellaneous	: constant string (1..13)	:= "miscellaneous";		

	
	project_name_max : constant natural := 100;
	package type_et_project_name is new generic_bounded_length (project_name_max);

	project_path_max : constant natural := 200;
	package type_et_project_path is new generic_bounded_length (project_path_max);

	-- The current project file name is stored here:
	package type_project_file_name is new generic_bounded_length (project_path_max + project_name_max + 1); -- incl. directory separator
	project_file_name : type_project_file_name.bounded_string;

	project_file_name_extension : constant string (1..2) := "et";
	
	project_file_handle : ada.text_io.file_type;

	-- The current directory where libraries live is stored here:
	package type_libraries_directory is new generic_bounded_length (project_path_max + directory_libraries'length + 1); -- incl. directory separator
	libraries_directory_name : type_libraries_directory.bounded_string;

	
	procedure create_libraries_directory (
	-- Creates a directory where libraries will live.
	-- An already existing directory will be overwritten.
		project_path	: in type_et_project_path.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);

	
	procedure create_project_directory (
	-- Creates given project directory in the given project_path.
	-- Already existing projects in given project_path are overwritten.
		project_name	: in type_et_project_name.bounded_string;
		project_path	: in type_et_project_path.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);


	
end et_project;

-- Soli Deo Gloria
