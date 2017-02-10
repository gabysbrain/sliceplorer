
namespace :appserver do
  desc <<-DESC
    set up virtualenv
    DESC
  task :venv do
    on roles(:app) do
      execute :mkdir, "-p", fetch(:venv_path, release_path)
      within fetch(:venv_path, release_path) do
        execute :python3, '-m', 'venv', '.'
      end
    end
  end

  desc <<-DESC
    install the required packages
    DESC
  task :setup => :venv do
    on roles(:app) do
      within fetch(:venv_path, release_path) do
        app_path = fetch(:app_path, release_path)
        execute "bin/pip", "install", "--upgrade", "-r", "#{app_path}/requirements_production.txt"
      end
    end
  end

  %w[start stop restart].each do |command|
    desc "#{command} gunicorn server."
    task command do
      on roles(:app) do
        execute :sudo, "systemctl", command, "slice_server"
        #execute "/etc/init.d/unicorn_#{fetch(:application)} #{command}"
      end
    end
  end

  namespace :cache do
    desc <<-DESC
      clear the data/clustering cache
      DESC
    task :clear do
      on roles(:app) do
        within fetch(:app_path, release_path) do
          execute :rm, "-f", "json_cache/*"
        end
      end
    end

    desc <<-DESC
      recreate the data/clustering cache (very slow)
      DESC
    task :create do
      on roles(:app) do
        within fetch(:app_path, release_path) do
          execute "./cache_all.sh"
        end
      end
    end

    desc <<-DESC
      refresh the data/clustering cache (very slow)
      DESC
    task :refresh => [:clear, :create]

    desc <<-DESC
      link the cache directory to the shared directory
      DESC
    task :link do
      on roles(:app) do
        within fetch(:app_path, release_path) do
          cachepath = "#{shared_path}/json_cache"
          execute :mkdir, "-p", cachepath
          execute :ln, "-s", cachepath, "json_cache"
        end
      end
    end
  end

end

