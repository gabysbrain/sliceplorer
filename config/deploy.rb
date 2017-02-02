# config valid only for current version of Capistrano
lock "3.7.2"

set :application, "sliceplorer"
set :repo_url, "git@github.com:gabysbrain/slicesplorer.git"

# Default branch is :master
# ask :branch, `git rev-parse --abbrev-ref HEAD`.chomp

# Default deploy_to directory is /var/www/my_app_name
set :deploy_to, "/home/torsnet6cs/sp_deploy"
set :deploy_via, :remote_cache

# Default value for :format is :airbrussh.
# set :format, :airbrussh

# You can configure the Airbrussh format using :format_options.
# These are the defaults.
# set :format_options, command_output: true, log_file: "log/capistrano.log", color: :auto, truncate: :auto

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
# append :linked_files, "config/database.yml", "config/secrets.yml"

# Default value for linked_dirs is []
# append :linked_dirs, "log", "tmp/pids", "tmp/cache", "tmp/sockets", "public/system"

# Default value for default_env is {}
# set :default_env, { path: "/opt/ruby/bin:$PATH" }

# Default value for keep_releases is 5
set :keep_releases, 3

# virtualenv setup
set :venv_path, -> { shared_path.join("venv") }
set :app_path, -> { release_path.join("sampling") }

# front-end setup
#set :npm_target_path, "#{release_path}/slice-viewer"
set :npm_target_path, -> { release_path.join('slice-viewer') }
set :npm_roles, :web
set :npm_flags, '--no-progress'
#set :bower_target_path, "#{release_path}/slice-viewer"

