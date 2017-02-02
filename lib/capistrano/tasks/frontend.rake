
namespace :frontend do

  desc <<-DESC
    Build the front-end code using npm run build. 
    DESC
  task :build do
    on roles(:web) do
      within fetch(:npm_target_path, release_path) do
        with fetch(:npm_env_variables, {}) do
          execute :npm, 'run', 'build'
        end
      end
    end
  end

end

